val launch :
          ?user_name:string ->
          ?password:string ->
          ?tool:string ->
          ?options:string
          -> Public_data.file_retriever
          -> string -> string -> int
