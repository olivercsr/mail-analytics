
type config = {
  host: string;
  port: int;
  collection: string;
} [@@deriving show]
;;

type db = {
  config: config;
} [@@deriving show]
;;

let new_db config =
  {config}
;;

