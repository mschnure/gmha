oral.suppression = extract.data(sim,"suppression.oral")
round(oral.suppression)
total.suppression = extract.data(sim,"suppression")
round(total.suppression)
lai.suppression = extract.data(sim,"suppression.lai")
round(lai.suppression)


aware = extract.data(sim,"awareness")

round(total.suppression/aware,3)
