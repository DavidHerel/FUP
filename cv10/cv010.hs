data Colors = Red | Green | Blue
 
instance Show Colors where
  show Red = "red"
  show Green = "green"
  show Blue = "blue"
 
instance Eq Colors where
  Red == Red = True
  Green == Green = True
  Blue == Blue = True
  _    == _    = False
