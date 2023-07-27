FROM debian:stable 
COPY pichunter_server pichunter_server
CMD ["pichunter_server"]
