{-# LANGUAGE DuplicateRecordFields #-}

module Cell (Cell, AgentCell, State, prettyPrintCell, cellToAgentCell) where

newtype AgentCell = AgentCell
  { state :: State
  }

data Cell = Cell
  { hasMine :: !Bool,
    state :: !State
  }

data State = Flagged | Unrevealed | Revealed

prettyPrintCell :: Cell -> String
prettyPrintCell Cell {state = Flagged} = "[F]"
prettyPrintCell Cell {state = Unrevealed} = "[U]"
prettyPrintCell Cell {hasMine = m} = if m then "[M]" else "[ ]"

cellToAgentCell :: Cell -> AgentCell
cellToAgentCell Cell {state = s} = AgentCell s
