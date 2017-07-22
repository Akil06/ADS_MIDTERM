FROM python



COPY WebScraping.py /

COPY config.json  /

COPY run.sh /



RUN pip install requests






RUN pip install pandas

RUN pip install lxml









ADD run.sh /

RUN chmod +x /run.sh

ENTRYPOINT ["python","./WebScraping.py"]