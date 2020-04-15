type cloud_client = NextCloudCmd
type file_retriever = WGET
type cloud_synchronization_mode = Daemon | CommandLine

type student_id =
  {
    firstname: string;
    lastname: string;
    promotion: string option;
  }
