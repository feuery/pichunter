FROM ubuntu:focal
COPY pichunter_server pichunter_server
COPY pichunter.min.js pichunter.min.js
COPY site.css site.css

CMD ["pichunter_server"]
