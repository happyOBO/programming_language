let rec range i1 i2 =
	if i1 > i2 then []
	else i1 :: range (i1+1) i2