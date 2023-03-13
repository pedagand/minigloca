let rec iterate_on vars_size fn i tests =
  MiniglocaLib.Ast.fl := 0;
  let vars = Array.make vars_size "" in
  let s = MiniglocaLib.Generator.generate vars 0 (Array.length vars) in
  fn (string_of_int i) s;
  
  if i < tests then iterate_on vars_size fn (i + 1) tests else ()