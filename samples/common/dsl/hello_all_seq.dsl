asl([task('HelloAWS',"arn:aws:lambda:us-east-2:410388484666:function:hello",
          [result_path('$.aws')]),
     task('HelloGCP',"grn:gcp:lambda:us-central1:glowing-program-196406:cloudfunctions.net:hello",
          [result_path('$.gcp')]),
     task('HelloAzure',"mrn:azure:lambda:japan-east:glowing-program-196406:azurewebsites.net:hello",
          [result_path('$.azure')]),
     task('HelloBluemix',"wsk:hello",
          [result_path('$.bluemix')])]).
