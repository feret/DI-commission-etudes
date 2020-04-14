type cloud_client = NextCloudCmd
type file_retriever = WGET
type cloud_synchronization_mode = Auto | Manual

type student_id =
  {
    firstname: string;
    lastname: string;
    promotion: string option;
  }
