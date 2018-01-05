
--create a specialized id function for strings
λ(String : *) ->
	(λ(a : *) -> λ(x : a) -> x) --Id function
	String
	
