open Core.Std

type mes = Send of int * Time.t
          |Receive of int * Time.t
          with sexp
