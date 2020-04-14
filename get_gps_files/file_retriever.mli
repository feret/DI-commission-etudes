val launch :
          ?user_name:string ->
          ?password:string ->
          ?tool:string ->
          ?options:string
          -> Public_data.file_retriever
          -> url:string
          -> output_repository:string
          -> output_file_name:string -> int
