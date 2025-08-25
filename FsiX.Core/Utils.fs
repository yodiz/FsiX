module FsiX.Utils

module Logging =
  let logInfo = printfn "\u001b[90m %s \u001b[0m"
  let logWarning = printfn "\u001b[33m %s \u001b[0m"
  let logError = printfn "\u001b[31m %s \u001b[0m"
  
