FROM oarfish/alpine-fsharp

COPY hello.fsx hello.fsx

CMD fsharpi oyster.fsx
