pub_sub
=====

An OTP application on pub-sub mechanism using gen_tcp communication between the client and server.
To compile the application please run below command from the terminal from project path
	"rebar3 compile"
To run the application please run below command
	"rebar3 shell"

pub_sub_manager is the client module, to start client and use it follow below steps
open erlang shell in the directory
run c(module name).
{ok, module name} is returned if its a success
call the module functions from the shell and establish server client communication.

