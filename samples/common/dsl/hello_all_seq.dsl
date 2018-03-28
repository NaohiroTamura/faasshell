asl([task('HelloAWS',"arn:aws:lambda:us-east-2:410388484666:function:hello",
          [result_path('$.aws')]),
     task('HelloGCP',"frn:gcp:functions:${gcp_location_id}:${gcp_project_id}:function:hello",
          [result_path('$.gcp')]),
     task('HelloAzure',"frn:azure:functions:${azure_location}:${azure_webapp_name}:function:hello",
          [result_path('$.azure')]),
     task('HelloBluemix',"frn:wsk:functions:::function:hello",
          [result_path('$.bluemix')])]).
