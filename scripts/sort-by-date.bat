find . ! -perm +a=w -printf "%%TY-%%Tm-%%Td %%TT %%p\n" | sort