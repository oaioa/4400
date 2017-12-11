pkill Client-exe
stack build
#stack exec -- Client-exe load-envs --name "PORT"
stack exec -- Client-exe $1
