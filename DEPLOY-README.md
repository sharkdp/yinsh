Use the bash scripts in this folder to deploy backend and frontend


on shark.fish:

  go to the ~/yinsh folder
  use `create-image.sh` to create a new docker image to run yinsh
  use `run.sh` to run the yinsh backend in the background
  use `docker ps`+`docker stop` to stop the backend
