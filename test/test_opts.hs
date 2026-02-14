import Agda.TypeChecking.Monad.Options
import Agda.Interaction.Options (defaultOptions)
import Agda.Utils.FileName (absolute)

main = do
  let opts = defaultOptions
  print "Options loaded"
