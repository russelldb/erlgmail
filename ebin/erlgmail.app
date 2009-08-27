{application, erlgmail,
 [{description, "Simple email sending application using gmail"},
  {vsn, "1.0"},
  {modules, [erlgmail, erlgmail_sup, new_smtp, erlgmail_app]},
  {registered, [erlgmail, erlgmail_sup]},
  {applications, [kernel, stdlib, ssl]},
  {mod, {erlgmail_app,[]}},
  {env, [{config_file, "erlgmail.cfg"}, {absolute, false}]}]}.