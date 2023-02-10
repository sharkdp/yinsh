FROM ubuntu:23.04
RUN mkdir /app
COPY ./yinsh-backend /app/yinsh-backend
RUN apt-get update && apt-get install -y openssl
CMD ["/app/yinsh-backend"]
