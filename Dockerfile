FROM erlang:alpine
RUN apk add --no-cache git ncurses-libs openssl libstdc++
RUN mkdir /app
WORKDIR /app
COPY . .
RUN rebar3 as prod release\
    chmod +x /app/_build/prod/rel/chat/bin/chat
EXPOSE 4000

CMD ["/app/_build/prod/rel/chat/bin/chat", "foreground"]