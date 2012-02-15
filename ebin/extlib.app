{application,extlib,
             [{description,"Extended Standard Library"},
              {vsn,"1.0.0"},
              {modules,[chash,chash_pg,dataset,erlopt,extbif,extlib_app,
                        extlib_sup,gen_server2,pipeline,prefix_exp,
                        priority_queue,sha1,uuid,varstr]},
              {registered,[chash_pg]},
              {applications,[kernel,stdlib,crypto]},
              {mod,{extlib_app,[]}},
              {env,[]}]}.