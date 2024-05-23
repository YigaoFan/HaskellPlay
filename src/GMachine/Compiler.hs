module GMachine.Compiler where
import AST (CoreProgram)
import GMachine.Util (GmState)

compile :: CoreProgram -> GmState