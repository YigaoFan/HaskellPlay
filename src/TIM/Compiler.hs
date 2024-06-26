module TIM.Compiler where

import AST (CoreProgram)
import TIM.Util (TimState)

compile :: CoreProgram -> TimState