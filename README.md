http - Opinionated HTTP/HTTPS for Erlang
=========================================

I had to interface with an api that took forever to return a few hundred k of  JSON
and so I wrote this, because httpc kept timing out and gun was a pain to use:

Getting Started
---------------

	http:start(),
	http:creds(),  %% if you have a creds file with your Authorization header
	Self = self(),
	http:then( fun(X) -> Self ! { json, json:parse(X) } end),
	http:get(<<"https://api.openai.com/v1/models">>, [ 'Authorization' ]).

The basic model of this is we start an http process, load our creds if needed,
register a callback, and then perform an http method: connect, delete, get,
head, options, patch, put, post, trace.  Depending on the method it takes

	http:method(Url)
	http:method(Url,Headers)
	http:method(Url,Headers,Body)

Currently only one http request can be in flight at a time, but I'll get
around to supporting multiple in-flight requests eventualy.  I'm still
not entirely decided on the callback handler, but I'll figure it out once
I unify this code with my websockets module.


MIT License

Copyright (c) 2023 David J Goehrig

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
