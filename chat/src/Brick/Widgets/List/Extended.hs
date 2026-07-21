module Brick.Widgets.List.Extended (
  listAppend,
) where

import Brick.Widgets.List
import Protolude

listAppend :: e -> List n e -> List n e
listAppend e l = listInsert len e l
 where
  len = length $ listElements l
