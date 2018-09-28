process :: String -> String
process input =
  let allLines = lines input
      [listaAdjacencias, pOnibus, [desiredWay]] = processaEntrada allLines
  in show desiredWay

-----------------------------------------

main = interact process
