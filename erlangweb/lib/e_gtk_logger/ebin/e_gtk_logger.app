%%% ===================================================================
%%% @author  Mijkenator <mijkenator@gmail.com>
%%% 
%%% ===================================================================
{application, e_gtk_logger,
 [{description, "e component for logging via error_logger"},
  {vsn, "1.0"},
  {modules, [e_gtk_logger]},
  {registered,[]},
  {env,[]},
  {applications, [kernel, stdlib, sasl, eptic]}]}.
