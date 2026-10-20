////////////////////////////////////////////////////////////////////////////////////////////////
// Description: C++ version of the core differential equations (compute.dx in model/diffeq.R)
//
// - The dynamics mirror compute.dx step by step, in the same order, so results should match the
//   R version to many decimal places.
// - The integrator is the one odeintr::integrate_sys uses: Boost odeint runge_kutta_dopri5 with
//   dense output, integrate_const, atol = rtol = 1e-6, observed once per step (1 year).
// - Time-varying parameters are interpolated exactly like compute.time.varying.parameters
//   (model/parameters.R). Each knot's value is expanded in R (prepare.cpp.model.spec in
//   model/diffeq_cpp.R) to the length it is used at, following R's recycling rules.
//
// Compiled from R with Rcpp::sourceCpp('model/diffeq_cpp.cpp') (done in model/diffeq_cpp.R).
// Needs the Rcpp and BH packages (both already installed as dependencies of odeintr).
//
// If compute.dx changes (new compartment, new transition, new parameter), this file needs the
// same change; prepare.cpp.model.spec stops with an error if the time-varying parameters no
// longer match what this file expects.
////////////////////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::depends(BH)]]

#include <vector>
#include <string>
#include <chrono>
#include <cmath>
#include <limits>
#include <algorithm>
#include <functional>
// only the parts of odeint we use (the umbrella header pulls in ublas and lots of deprecation warnings)
#include <boost/numeric/odeint/stepper/runge_kutta_dopri5.hpp>
#include <boost/numeric/odeint/stepper/generation.hpp>
#include <boost/numeric/odeint/integrate/integrate_const.hpp>

namespace gmha {

typedef std::vector<double> state_type;

// Thrown when a run exceeds max.run.seconds (the R version returns NA derivatives instead;
// either way the remaining output is NA)
struct TimeoutError {};

//-- ONE TIME-VARYING PARAMETER --//
// knot times + the value at each knot, expanded to `len` and stored contiguously:
// knot k occupies values[k*len, (k+1)*len)
struct TimeVaryingParam {
    std::vector<double> times;
    std::vector<double> values;
    std::size_t len = 0;
    std::vector<double> buffer; // holds interpolated values
    std::vector<double> slopes; // (v2 - v1) / (t2 - t1) for each segment, computed once in finalize()

    // Call after times/values/len are filled
    void finalize() {
        buffer.assign(len, 0.0);
        const std::size_t n = times.size();
        slopes.assign(n > 1 ? (n - 1) * len : 0, 0.0);
        for (std::size_t k = 0; k + 1 < n; ++k)
            for (std::size_t j = 0; j < len; ++j)
                slopes[k * len + j] = (values[(k + 1) * len + j] - values[k * len + j]) / (times[k + 1] - times[k]);
    }

    // Mirrors compute.time.varying.parameters() in model/parameters.R
    const double* at(double t) {
        const std::size_t n = times.size();
        if (t <= times[0])
            return &values[0];                    // first value
        if (t >= times[n - 1])
            return &values[(n - 1) * len];        // last value
        // last index whose time is <= t (times are sorted)
        const std::size_t i1 = (std::upper_bound(times.begin(), times.end(), t) - times.begin()) - 1;
        if (t == times[i1])
            return &values[i1 * len];
        // linear interpolation: value1 + slope * (time - time1), same arithmetic as the R code
        const double dt = t - times[i1];
        const double* v1 = &values[i1 * len];
        const double* slope = &slopes[i1 * len];
        for (std::size_t j = 0; j < len; ++j)
            buffer[j] = v1[j] + slope[j] * dt;
        return buffer.data();
    }
};

//-- DIMENSIONS AND INDICES (all indices 0-based) --//
struct Dims {
    int n_ages = 0, n_sexes = 0, n_subgroups = 0, n_hiv_status = 0;
    int male = 0, female = 1;
    int hiv_negative = 0, undiagnosed = 1, diagnosed_unengaged = 2,
        engaged_unsuppressed = 3, engaged_suppressed = 4;
    int lai_art = -1;   // NEXTGEN: index of the lai_art status; -1 if the model has no LAI
};

//-- THE MODEL: holds parameters + workspace; operator() is the derivative function --//
struct Model {
    Dims d;
    std::size_t T = 0;   // trans length: ages * sexes * subgroups
    std::size_t S = 0;   // state length: T * hiv statuses
    std::size_t P = 0;   // prevalence length: T * (hiv statuses - 1)
    std::size_t total_length = 0;

    // offsets into the flattened vector, same order as the rv vector in compute.dx
    std::size_t o_state = 0, o_prev = 0, o_inc = 0, o_diag = 0, o_hiv_mort = 0, o_non_hiv_mort = 0,
                o_total_mort = 0, o_engagement = 0, o_disengagement_unsuppressed = 0,
                o_disengagement_suppressed = 0, o_suppression = 0,
                o_lai_total = 0, o_lai_es = 0, o_lai_eu = 0, o_lai_du = 0, o_lai_removal = 0; // NEXTGEN
    bool has_lai = false; // NEXTGEN

    TimeVaryingParam fertility, maternal_fetal, male_births, aging, hiv_mortality, non_hiv_mortality,
                     testing, transmission, infectiousness, engagement, unsuppressed_disengagement,
                     suppressed_disengagement, suppression, unsuppression,
                     lai_es, lai_eu, lai_du, lai_removal; // NEXTGEN

    // workspace
    std::vector<double> hiv_births_g, non_hiv_births_g;
    std::vector<double> aging_flow;
    std::vector<double> infectiousness_from, incidence_rate;

    // run-time limit (max.run.seconds in run.model)
    double max_seconds = std::numeric_limits<double>::infinity();
    std::chrono::steady_clock::time_point start_clock = std::chrono::steady_clock::now();

    void setup() {
        T = (std::size_t)d.n_ages * d.n_sexes * d.n_subgroups;
        S = T * d.n_hiv_status;
        P = T * (d.n_hiv_status - 1);

        o_state = 0;
        o_prev = o_state + S;
        o_inc = o_prev + P;
        o_diag = o_inc + T;
        o_hiv_mort = o_diag + T;
        o_non_hiv_mort = o_hiv_mort + S;
        o_total_mort = o_non_hiv_mort + S;
        o_engagement = o_total_mort + S;
        o_disengagement_unsuppressed = o_engagement + T;
        o_disengagement_suppressed = o_disengagement_unsuppressed + T;
        o_suppression = o_disengagement_suppressed + T;
        total_length = o_suppression + T;
        // NEXTGEN: lai.art (net), from engaged_suppressed, engaged_unsuppressed, diagnosed_unengaged, and removal
        has_lai = d.lai_art >= 0;
        if (has_lai) {
            o_lai_total = total_length;
            o_lai_es = o_lai_total + T;
            o_lai_eu = o_lai_es + T;
            o_lai_du = o_lai_eu + T;
            o_lai_removal = o_lai_du + T;
            total_length = o_lai_removal + T;
        }

        hiv_births_g.assign(d.n_subgroups, 0.0);
        non_hiv_births_g.assign(d.n_subgroups, 0.0);
        aging_flow.assign(S, 0.0);
        infectiousness_from.assign(T, 0.0);
        incidence_rate.assign(T, 0.0);
    }

    // index helpers (column-major, same layout as the R arrays)
    inline std::size_t tidx(int a, int s, int g) const {            // [age, sex, subgroup]
        return (std::size_t)a + (std::size_t)d.n_ages * ((std::size_t)s + (std::size_t)d.n_sexes * (std::size_t)g);
    }
    inline std::size_t sidx(std::size_t trans_index, int h) const {  // [age, sex, subgroup, hiv.status]
        return trans_index + T * (std::size_t)h;
    }

    void check_time() const {
        if (std::isfinite(max_seconds)) {
            const double elapsed =
                std::chrono::duration<double>(std::chrono::steady_clock::now() - start_clock).count();
            if (elapsed > max_seconds)
                throw TimeoutError();
        }
    }

    // A simple flow between two hiv statuses: flow = rate * state[,,,from]
    //   dx.state[,,,from] -= flow; dx.state[,,,to] += flow; tracked (if not null) = flow
    inline void status_flow(const double* rate, const double* st, double* dx,
                            int from, int to, double* tracked) const {
        for (std::size_t j = 0; j < T; ++j) {
            const double flow = rate[j] * st[sidx(j, from)];
            dx[sidx(j, from)] -= flow;
            dx[sidx(j, to)] += flow;
            if (tracked) tracked[j] = flow;
        }
    }

    // The derivative function: mirrors compute.dx (model/diffeq.R)
    void operator()(const state_type& y, state_type& dydt, double t) {
        check_time();

        //-- GET TIME-VARYING PARAMETERS --//
        const double* FERTILITY      = fertility.at(t);
        const double* MFT            = maternal_fetal.at(t);
        const double  MALE_BIRTHS    = male_births.at(t)[0];
        const double* AGING          = aging.at(t);
        const double* HIV_MORT       = hiv_mortality.at(t);
        const double* NON_HIV_MORT   = non_hiv_mortality.at(t);
        const double* TESTING        = testing.at(t);
        const double* TRANSMISSION   = transmission.at(t);   // T x T, column-major, [to, from]
        const double* INFECTIOUSNESS = infectiousness.at(t);
        const double* ENGAGEMENT     = engagement.at(t);
        const double* UNSUPP_DISENG  = unsuppressed_disengagement.at(t);
        const double* SUPP_DISENG    = suppressed_disengagement.at(t);
        const double* SUPPRESSION    = suppression.at(t);
        const double* UNSUPPRESSION  = unsuppression.at(t);

        //-- SET UP DX ARRAYS --//
        std::fill(dydt.begin(), dydt.end(), 0.0);
        const double* st = y.data();                 // state is the first S elements of y
        double* dx         = dydt.data() + o_state;
        double* d_prev     = dydt.data() + o_prev;
        double* d_inc      = dydt.data() + o_inc;
        double* d_diag     = dydt.data() + o_diag;
        double* d_hiv_mort = dydt.data() + o_hiv_mort;
        double* d_non_hiv  = dydt.data() + o_non_hiv_mort;
        double* d_tot_mort = dydt.data() + o_total_mort;
        double* d_eng      = dydt.data() + o_engagement;
        double* d_dis_uns  = dydt.data() + o_disengagement_unsuppressed;
        double* d_dis_sup  = dydt.data() + o_disengagement_suppressed;
        double* d_supp     = dydt.data() + o_suppression;

        const int nA = d.n_ages, nS = d.n_sexes, nG = d.n_subgroups, nH = d.n_hiv_status;

        //-- BIRTH --//
        // births = state * FERTILITY; hiv.births = births * MFT; summed over age, sex, hiv status
        // within each subgroup (apply(., 3, sum))
        for (int g = 0; g < nG; ++g) {
            long double hb = 0.0L, nhb = 0.0L;
            for (int h = 0; h < nH; ++h)
                for (int s = 0; s < nS; ++s)
                    for (int a = 0; a < nA; ++a) {
                        const std::size_t i = sidx(tidx(a, s, g), h);
                        const double births = st[i] * FERTILITY[i];
                        hb  += births * MFT[i];
                        nhb += births * (1 - MFT[i]);
                    }
            hiv_births_g[g] = (double)hb;
            non_hiv_births_g[g] = (double)nhb;
        }
        for (int g = 0; g < nG; ++g) {
            dx[sidx(tidx(0, d.male, g),   d.undiagnosed)]  = hiv_births_g[g] * MALE_BIRTHS;
            dx[sidx(tidx(0, d.female, g), d.undiagnosed)]  = hiv_births_g[g] * (1 - MALE_BIRTHS);
            dx[sidx(tidx(0, d.male, g),   d.hiv_negative)] = non_hiv_births_g[g] * MALE_BIRTHS;
            dx[sidx(tidx(0, d.female, g), d.hiv_negative)] = non_hiv_births_g[g] * (1 - MALE_BIRTHS);
        }

        //-- AGING --//
        for (std::size_t i = 0; i < S; ++i) {
            aging_flow[i] = st[i] * AGING[i];
            dx[i] -= aging_flow[i];                                  // aging out
        }
        for (int h = 0; h < nH; ++h)
            for (int g = 0; g < nG; ++g)
                for (int s = 0; s < nS; ++s) {
                    for (int a = 1; a < nA; ++a)                     // aging into next age group
                        dx[sidx(tidx(a, s, g), h)] += aging_flow[sidx(tidx(a - 1, s, g), h)];
                    const std::size_t last = sidx(tidx(nA - 1, s, g), h);
                    d_non_hiv[last] += aging_flow[last];             // oldest age group ages out of the model
                }

        //-- MORTALITY --//
        for (std::size_t i = 0; i < S; ++i) {
            const double hm = st[i] * HIV_MORT[i];
            dx[i] -= hm;
            d_hiv_mort[i] += hm;
        }
        for (std::size_t i = 0; i < S; ++i) {
            const double nhm = st[i] * NON_HIV_MORT[i];
            dx[i] -= nhm;
            d_non_hiv[i] += nhm;
        }
        for (std::size_t i = 0; i < S; ++i)
            d_tot_mort[i] = d_hiv_mort[i] + d_non_hiv[i];

        //-- DIAGNOSES --//
        status_flow(TESTING, st, dx, d.undiagnosed, d.diagnosed_unengaged, d_diag);

        //-- INCIDENCE --//
        // infectiousness of each [age, sex, subgroup] = sum over hiv status of
        //   (proportion of the stratum in that status) * INFECTIOUSNESS
        for (std::size_t j = 0; j < T; ++j) {
            long double total = 0.0L;
            for (int h = 0; h < nH; ++h)
                total += st[sidx(j, h)];
            long double inf = 0.0L;
            for (int h = 0; h < nH; ++h)
                inf += (st[sidx(j, h)] / (double)total) * INFECTIOUSNESS[sidx(j, h)];
            infectiousness_from[j] = (double)inf;
        }
        // TRANSMISSION.RATES %*% infectiousness.from
        std::fill(incidence_rate.begin(), incidence_rate.end(), 0.0);
        for (std::size_t from = 0; from < T; ++from) {
            const double inf_from = infectiousness_from[from];
            const double* col = TRANSMISSION + from * T;
            for (std::size_t to = 0; to < T; ++to)
                incidence_rate[to] += col[to] * inf_from;
        }
        for (std::size_t j = 0; j < T; ++j)
            d_inc[j] = st[sidx(j, d.hiv_negative)] * incidence_rate[j];

        //-- NEW INFECTIONS --//
        for (std::size_t j = 0; j < T; ++j) {
            dx[sidx(j, d.hiv_negative)] -= d_inc[j];
            dx[sidx(j, d.undiagnosed)]  += d_inc[j];
        }
        // record new HIV births as new infections
        for (int g = 0; g < nG; ++g) {
            d_inc[tidx(0, d.male, g)]   += hiv_births_g[g] * MALE_BIRTHS;
            d_inc[tidx(0, d.female, g)] += hiv_births_g[g] * (1 - MALE_BIRTHS);
        }

        //-- ENGAGEMENT --//
        status_flow(ENGAGEMENT, st, dx, d.diagnosed_unengaged, d.engaged_unsuppressed, d_eng);

        //-- DISENGAGED FROM UNSUPPRESSED --//
        status_flow(UNSUPP_DISENG, st, dx, d.engaged_unsuppressed, d.diagnosed_unengaged, d_dis_uns);

        //-- DISENGAGED FROM SUPPRESSED --//
        status_flow(SUPP_DISENG, st, dx, d.engaged_suppressed, d.diagnosed_unengaged, d_dis_sup);

        //-- SUPPRESSION --//
        status_flow(SUPPRESSION, st, dx, d.engaged_unsuppressed, d.engaged_suppressed, d_supp);

        //-- UNSUPPRESSION --// (not tracked)
        status_flow(UNSUPPRESSION, st, dx, d.engaged_suppressed, d.engaged_unsuppressed, nullptr);

        //-- LAI ART (NEXTGEN) --// same flows as compute.dx: three arrows onto LAI, and removal back to engaged_suppressed
        if (has_lai) {
            double* d_lai_total = dydt.data() + o_lai_total;
            double* d_lai_es    = dydt.data() + o_lai_es;
            double* d_lai_eu    = dydt.data() + o_lai_eu;
            double* d_lai_du    = dydt.data() + o_lai_du;
            double* d_lai_rem   = dydt.data() + o_lai_removal;
            status_flow(lai_es.at(t),      st, dx, d.engaged_suppressed,   d.lai_art, d_lai_es);
            status_flow(lai_eu.at(t),      st, dx, d.engaged_unsuppressed, d.lai_art, d_lai_eu);
            status_flow(lai_du.at(t),      st, dx, d.diagnosed_unengaged,  d.lai_art, d_lai_du);
            status_flow(lai_removal.at(t), st, dx, d.lai_art, d.engaged_suppressed,   d_lai_rem);
            for (std::size_t j = 0; j < T; ++j)
                d_lai_total[j] = d_lai_es[j] + d_lai_eu[j] + d_lai_du[j] - d_lai_rem[j];
        }

        //-- RECORD PREVALENCE --// dx.prevalence = dx.state[,,,-1]
        std::copy(dx + T, dx + S, d_prev);
    }
};

//-- SYSTEM WRAPPER: odeint copies the system it is given, so pass a light handle to the model --//
struct SystemRef {
    Model* model;
    void operator()(const state_type& x, state_type& dxdt, double t) const { (*model)(x, dxdt, t); }
};

//-- OBSERVER: records the full vector once per step --//
struct Observer {
    std::vector<double>& times;
    std::vector<state_type>& rows;
    void operator()(const state_type& x, double t) {
        times.push_back(t);
        rows.push_back(x);
    }
};

// Integrates the model the same way odeintr::integrate_sys does.
// Returns true if it finished, false if it hit the time limit (rows recorded so far are kept).
inline bool integrate(Model& model, state_type init, double start, double duration,
                      double step_size, double atol, double rtol,
                      std::vector<double>& times, std::vector<state_type>& rows) {
    namespace odeint = boost::numeric::odeint;
    typedef odeint::runge_kutta_dopri5<state_type> stepper_type;
    model.start_clock = std::chrono::steady_clock::now();
    Observer obs{times, rows};
    auto stepper = odeint::make_dense_output(atol, rtol, stepper_type());
    try {
        odeint::integrate_const(stepper, SystemRef{&model}, init, start, start + duration, step_size, obs);
    } catch (const TimeoutError&) {
        return false;
    }
    return true;
}

} // namespace gmha


#ifndef GMHA_STANDALONE
////////////////////////////////////////////////////////////////////////////////////////////////
// R interface
////////////////////////////////////////////////////////////////////////////////////////////////
#include <Rcpp.h>

static void fill_param(gmha::TimeVaryingParam& p, Rcpp::List params, const char* name,
                       std::size_t expected_len) {
    if (!params.containsElementNamed(name))
        Rcpp::stop(std::string("missing time-varying parameter: ") + name);
    Rcpp::List p_r = params[name];
    Rcpp::NumericVector times = p_r["times"];
    Rcpp::NumericMatrix values = p_r["values"];  // len x n.knots
    if ((std::size_t)values.nrow() != expected_len)
        Rcpp::stop(std::string("wrong length for ") + name + ": expected " + std::to_string(expected_len) +
                   ", got " + std::to_string(values.nrow()));
    if (values.ncol() != times.size() || times.size() == 0)
        Rcpp::stop(std::string("number of knot times and values differ (or are 0) for ") + name);
    p.times.assign(times.begin(), times.end());
    p.values.assign(values.begin(), values.end());
    p.len = expected_len;
    p.finalize();
}

static void build_model(gmha::Model& m, Rcpp::List spec) {
    Rcpp::IntegerVector dims = spec["dims"];
    m.d.n_ages               = dims["n.ages"];
    m.d.n_sexes              = dims["n.sexes"];
    m.d.n_subgroups          = dims["n.subgroups"];
    m.d.n_hiv_status         = dims["n.hiv.status"];
    m.d.male                 = dims["male"];
    m.d.female               = dims["female"];
    m.d.hiv_negative         = dims["hiv_negative"];
    m.d.undiagnosed          = dims["undiagnosed"];
    m.d.diagnosed_unengaged  = dims["diagnosed_unengaged"];
    m.d.engaged_unsuppressed = dims["engaged_unsuppressed"];
    m.d.engaged_suppressed   = dims["engaged_suppressed"];
    // NEXTGEN: lai_art is only in dims when the model has the LAI state
    Rcpp::CharacterVector dim_names = dims.names();
    m.d.lai_art = -1;
    for (int k = 0; k < dim_names.size(); ++k)
        if (Rcpp::as<std::string>(dim_names[k]) == "lai_art") m.d.lai_art = dims[k];
    m.setup();

    const std::size_t S = m.S, T = m.T;
    Rcpp::List params = spec["params"];
    fill_param(m.fertility,                  params, "FERTILITY.RATES", S);
    fill_param(m.maternal_fetal,             params, "MATERNAL.FETAL.TRANSMISSION", S);
    fill_param(m.male_births,                params, "MALE.BIRTHS", 1);
    fill_param(m.aging,                      params, "AGING.RATES", S);
    fill_param(m.hiv_mortality,              params, "HIV.MORTALITY.RATES", S);
    fill_param(m.non_hiv_mortality,          params, "NON.HIV.MORTALITY.RATES", S);
    fill_param(m.testing,                    params, "TESTING.RATES", T);
    fill_param(m.transmission,               params, "TRANSMISSION.RATES", T * T);
    fill_param(m.infectiousness,             params, "INFECTIOUSNESS.H", S);
    fill_param(m.engagement,                 params, "ENGAGEMENT.RATES", T);
    fill_param(m.unsuppressed_disengagement, params, "UNSUPPRESSED.DISENGAGEMENT.RATES", T);
    fill_param(m.suppressed_disengagement,   params, "SUPPRESSED.DISENGAGEMENT.RATES", T);
    fill_param(m.suppression,                params, "SUPPRESSION.RATES", T);
    fill_param(m.unsuppression,              params, "UNSUPPRESSION.RATES", T);
    if (m.has_lai) { // NEXTGEN
        fill_param(m.lai_es,                 params, "LAI.ES.RATES", T);
        fill_param(m.lai_eu,                 params, "LAI.EU.RATES", T);
        fill_param(m.lai_du,                 params, "LAI.DU.RATES", T);
        fill_param(m.lai_removal,            params, "LAI.REMOVAL.RATES", T);
    }

    m.max_seconds = Rcpp::as<double>(spec["max.run.seconds"]);
}

// Runs the model; returns a matrix laid out like as.matrix(odeintr::integrate_sys(...)):
// first column Time, then X1..Xn, one row per year from start to start + duration
// [[Rcpp::export]]
Rcpp::NumericMatrix gmha_run_model_cpp(Rcpp::List spec, Rcpp::NumericVector init,
                                       double start, double duration,
                                       double step_size = 1.0, double atol = 1e-6, double rtol = 1e-6) {
    gmha::Model m;
    build_model(m, spec);
    if ((std::size_t)init.size() != m.total_length)
        Rcpp::stop("initial vector has length " + std::to_string(init.size()) + ", expected " +
                   std::to_string(m.total_length));

    std::vector<double> times;
    std::vector<gmha::state_type> rows;
    const std::size_t expected_rows = (std::size_t)std::floor(duration / step_size + 1e-9) + 1;
    times.reserve(expected_rows);
    rows.reserve(expected_rows);

    gmha::state_type x0(init.begin(), init.end());
    bool finished;
    try {
        finished = gmha::integrate(m, x0, start, duration, step_size, atol, rtol, times, rows);
    } catch (std::exception& e) {
        Rcpp::stop(std::string("C++ integrator error: ") + e.what());
    }

    // if the run timed out, pad the remaining years with NA (the R version also ends up NA)
    const std::size_t n_rows = finished ? rows.size() : std::max(expected_rows, rows.size());
    const std::size_t n = m.total_length;
    Rcpp::NumericMatrix out(n_rows, n + 1);
    for (std::size_t r = 0; r < n_rows; ++r) {
        if (r < rows.size()) {
            out(r, 0) = times[r];
            for (std::size_t j = 0; j < n; ++j) out(r, j + 1) = rows[r][j];
        } else {
            out(r, 0) = start + r * step_size;
            for (std::size_t j = 0; j < n; ++j) out(r, j + 1) = NA_REAL;
        }
    }
    Rcpp::CharacterVector cn(n + 1);
    cn[0] = "Time";
    for (std::size_t j = 0; j < n; ++j) cn[j + 1] = "X" + std::to_string(j + 1);
    Rcpp::colnames(out) = cn;
    return out;
}

// Evaluates the derivative once (for comparing against compute.dx in R)
// [[Rcpp::export]]
Rcpp::NumericVector gmha_compute_dx_cpp(Rcpp::List spec, double time, Rcpp::NumericVector y) {
    gmha::Model m;
    build_model(m, spec);
    m.max_seconds = std::numeric_limits<double>::infinity();
    if ((std::size_t)y.size() != m.total_length)
        Rcpp::stop("y has length " + std::to_string(y.size()) + ", expected " + std::to_string(m.total_length));
    gmha::state_type x(y.begin(), y.end()), dxdt(m.total_length, 0.0);
    m(x, dxdt, time);
    return Rcpp::NumericVector(dxdt.begin(), dxdt.end());
}
#endif
