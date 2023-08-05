FROM ubuntu:latest
COPY . /src
RUN cd /src; \
    ls -la . ; \
    ./build_pichunter.sh \
    cd ..

COPY pichunter.min.js pichunter.min.js
COPY pichunter-helper.js pichunter-helper.js
COPY site.css site.css

CMD ["/pichunter_server"]
