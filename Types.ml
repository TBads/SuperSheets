type user = {
  username : string option;
  email    : string option;
  verified : bool option
} deriving (Json)
