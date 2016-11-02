FROM oarfish/alpine-fsharp

COPY oyster.fsx oyster.fsx

CMD fsharpi oyster.fsx
