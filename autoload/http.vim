let s:save_cpo = &cpo
set cpo&vim

let s:V = vital#vim_http#new()
let s:Base64 = s:V.import('Data.Base64')
let s:Promise = s:V.import('Async.Promise')

function! s:new_request() abort
    let l:response = {
                \ 'method': 'GET',
                \ 'headers': {},
                \ 'uri': '',
                \ 'content': '',
                \ 'version': '1.1',
                \ 'meta': {
                \   'follow': 0,
                \   }
                \ }
    return l:response
endfunction

let s:pre_clean_uri_line_pattern = '^\(\a*\)\s*\(\S*\)\s*\(HTTP/\([0-9.]\+\)\)\?.*$'
let s:uri_line_pattern = '^\(OPTIONS\|GET\|HEAD\|POST\|PUT\|DELETE\|TRACE\|CONNECT\|PATCH\)\s*\(\S*\)\s*\(HTTP/\([0-9.]\+\)\)\?.*$'
let s:header_line_pattern = '^\([^:]\+\): \(.*\)$'
let s:request_line_idx = 0
let s:request_separator = '^###'

function! s:get_lines(buffer, count, line1, line2) abort
    if a:count == -1
      let lines = getbufline(a:buffer, 0, '$')
    else
      let lines = getbufline(a:buffer, a:line1, a:line2)
    endif
    let cur_idx = getbufinfo('')[0].lnum - 1
    let offset = cur_idx - 1
    while offset > -1 && match(lines[offset], s:request_separator) == -1
      let offset -= 1
    endwhile
    if offset > 0
      call remove(lines, 0, offset)
    endif
    let below = match(lines, s:request_separator)
    if below > -1
      call remove(lines, below, len(lines) - 1)
    endif
    let req_idx = match(lines, s:uri_line_pattern)
    let s:request_line_idx = req_idx > -1 ? offset + req_idx + 2 : -1
    if req_idx > 0
      call remove(lines, 0, req_idx - 1)
    endif
    return lines
endfunction

function! s:parse_request(lines, pattern, follow) abort
    let l:request = s:new_request()
    if a:follow == 1
        let l:request.meta.follow = 1
    end

    if len(a:lines) < 0
        throw 'No lines in buffer :('
    endif

    let l:uri_line = a:lines[0]
    let l:uri_line_matches = matchlist(l:uri_line, a:pattern)

    if len(l:uri_line_matches) == 0 || s:request_line_idx == -1
        throw 'Unable to parse first line of request'
    end

    let l:request.method = l:uri_line_matches[1]
    let l:request.uri = l:uri_line_matches[2]
    let l:request.version = l:uri_line_matches[4]

    if l:request.version =~ '2.*'
      let l:request.version = '2'
    end

    let l:in_content = 0
    let l:first_content_line = 1
    for l:line in a:lines[0:]
        if l:in_content == 0 && l:line =~ '^\s*$'
            let l:in_content = 1
        end
        if l:in_content == 0
            let l:header_matches = matchlist(l:line, s:header_line_pattern)

            if len(l:header_matches) > 0
                let l:header_key = l:header_matches[1]
                let l:header_value = l:header_matches[2]
                if has_key(l:request.headers, l:header_key) == 0
                    let l:request.headers[l:header_key] = []
                endif
                let l:request.headers[l:header_key] += [l:header_value]
            endif
        else
            if l:first_content_line && l:line =~ '^\s*$'
                continue
            endif
            if l:first_content_line
                let l:first_content_line = 0
            else
                let l:request.content .= "\r\n"
            endif
            let l:request.content .= l:line
        endif
    endfor

    return l:request
endfunction

function! s:in_curl_format(request) abort
    let l:curl_fmt = 'curl%s%s%s%s %s'

    let l:flags = ' -si'

    if a:request.meta.follow == 1
        let l:flags = l:flags . 'L'
    endif

    let l:flags = l:flags.' '.g:vim_http_additional_curl_args

    let l:flags = l:flags.' --http'.a:request.version

    let l:method = printf(' -X %s', a:request.method)

    let l:url = shellescape(a:request.uri)

    let l:headers = ''

    for [l:header_key, l:header_value_list] in items(a:request.headers)
        for l:header_value in l:header_value_list
            let l:headers = l:headers .
                        \ ' -H "' .
                        \ printf('%s: %s', l:header_key, l:header_value) .
                        \ '"'
        endfor

    endfor

    let l:data = ''
    if len(a:request.content)
        let l:data = ' -d ' . substitute(shellescape(a:request.content), '\\\n', "\n", 'g')
    endif

    let l:curl_request = printf(l:curl_fmt, l:flags, l:method, l:headers, l:data, l:url)

    return l:curl_request

endfunction

function! s:lines_with_header(header) abort
  let l:linenr = 1
  let l:lines = []
  while l:linenr < line('$')
    let l:line = getline(l:linenr)
    if l:line =~ '^\s*$'
      break
    endif
    if l:line =~ "^".a:header.":"
      call add(l:lines, l:linenr)
    end
    let l:linenr = l:linenr + 1
  endwhile
  return l:lines
endfunction

function! s:remove_header(header) abort
  let l:offset = 0
  for l:linenr in s:lines_with_header(a:header)
    let l:target = l:linenr - l:offset
    exe l:target."delete _"
    let l:offset = l:offset + 1
  endfor
endfunction

function! s:new_response_buffer(request_buffer) abort
    let l:request_buffer_name  = bufname(a:request_buffer)
    let l:buffer_name = fnamemodify(l:request_buffer_name, ":r") . '.response.' . localtime() . '.http'
    if g:vim_http_tempbuffer
      for l:win in range(1, winnr('$'))
        if getwinvar(l:win, 'vim_http_tempbuffer')
          execute l:win . 'windo close'
        endif
      endfor
    endif
    let l:keepalt = g:vim_http_tempbuffer ? 'keepalt ' : ''
    let l:vert = g:vim_http_split_vertically ? 'vert ' : ''
    execute l:keepalt . l:vert . 'new ' . l:buffer_name
    set ft=http
    if g:vim_http_tempbuffer
      setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile nonumber
      let w:vim_http_tempbuffer = 1
    endif
    let l:new_buffer = bufnr('')
    if g:vim_http_split_vertically
      wincmd l
    else
      wincmd k
    endif
    return l:new_buffer
endfunction

function! http#do_buffer(follow) abort
    let l:bang =  a:follow ? '!' : ''
    call http#do(l:bang, -1, 0, 0)
endfunction

function! s:exec(cmd, options, resolve, reject) abort
  function! s:read(chan, data, part) abort closure
    let data = join(a:data, "\n")
    if a:part == 'out' && !empty(data)
      call a:resolve(data)
    elseif a:part == 'err'
      call a:reject('Something went wrong')
    endif
  endfunction
  call jobstart(a:cmd, {
        \   'detach'    : v:true,
        \   'on_stdout' : {ch, data, event -> s:read(ch, data, 'out')},
        \   'on_stderr' : {ch, data, event -> s:read(ch, data, 'err')},
        \   })
endfunction

function! s:sh(cmd) abort
  return s:Promise.new(funcref('s:exec', [a:cmd, {}]))
endfunction

function! s:update_response_buffer(bufnr, lines) abort
  let l:size = len(getbufline(a:bufnr, 1, '$'))
  if !g:vim_http_tempbuffer && l:size > 0 && !g:vim_http_appendbuf 
    call setbufline(a:bufnr, 1, repeat([''], l:size))
  endif
  call appendbufline(a:bufnr, 0, a:lines)
endfunction

function! http#do(bang, count, line1, line2) abort
    let l:follow =  a:bang == '!' ? 1 : 0
    if g:vim_http_clean_before_do && a:count == -1
      call http#clean()
    end
    let l:buffer = bufnr('')
    let l:lines = s:get_lines(l:buffer, a:count, a:line1, a:line2)
    let l:request = s:parse_request(l:lines, s:uri_line_pattern, l:follow)
    let l:curl = s:in_curl_format(l:request)
    let l:new_bufnr = s:new_response_buffer(l:buffer)
    func! s:wrap_upd_buf(data) closure
      call s:update_response_buffer(l:new_bufnr, a:data)
    endfunc
    call s:sh(l:curl)
          \.then({ data -> split(data, "\\(\r\n\\|\n\\)") })
          \.then(funcref('s:wrap_upd_buf'))
endfunction

function! http#show_curl(bang, count, line1, line2) abort
    let l:follow =  a:bang == '!' ? 1 : 0
    let l:buffer = bufnr('')
    let l:lines = s:get_lines(l:buffer, a:count, a:line1, a:line2)
    let l:request = s:parse_request(l:lines, s:uri_line_pattern, l:follow)
    let l:curl = s:in_curl_format(l:request)
    echo l:curl
endfunction

function! http#show_request(bang, count, line1, line2) abort
    let l:follow =  a:bang == '!' ? 1 : 0
    let l:buffer = bufnr('')
    let l:lines = s:get_lines(l:buffer, a:count, a:line1, a:line2)
    let l:request = s:parse_request(l:lines, s:uri_line_pattern, l:follow)
    echo l:request
endfunction

function! http#clean() abort
  let l:buffer = bufnr('')
  let l:lines = s:get_lines(l:buffer, -1, 0, 0)
  let l:request = s:parse_request(l:lines, s:pre_clean_uri_line_pattern, 0)

  " when the http proto is > 1.0 make sure we are adding a host header
  if index(['1.1', '2'], l:request.version) != -1 && !has_key(l:request.headers, 'Host')
    let l:matches = matchlist(l:request.uri, '^\([^:]\+://\)\?\([^/]\+\)')
    let l:host = l:matches[2]
    if len(l:host)
      call append(s:request_line_idx, 'Host: ' . l:host)
    endif
  endif

  if l:request.version == ''
    call setline(s:request_line_idx, getline(s:request_line_idx) . ' HTTP/1.1')
  endif

  let l:content_length = len(l:request.content)

  " when we have a Content-Length header and it doesn't match the actual
  " content length
  if l:content_length && has_key(l:request.headers, 'Content-Length')
    if string(l:content_length) != l:request.headers['Content-Length'][-1]
      let l:correct = input("correct Content-Length header? [Y]/N:")
      if len(l:correct) == 0 || tolower(l:correct) != "n"
        call remove(l:request.headers, 'Content-Length')
        call s:remove_header('Content-Length')
      endif
    endif
  endif

  " when we are sending content we should add a header for the content length
  " curl is going to do this for us anyway, but it's good to be explicit
  if  l:content_length && !has_key(l:request.headers, 'Content-Length')
    call append(s:request_line_idx + len(l:request.headers), 'Content-Length: ' . l:content_length)
  endif
endfunction

function! http#auth() abort
  let l:buffer = bufnr('')
  let l:lines = s:get_lines(l:buffer, -1, 0, 0)
  let l:request = s:parse_request(l:lines, s:uri_line_pattern, 0)

  let l:method = input('method [Basic]: ')
  if len(l:method) == 0
    let l:method = 'Basic'
  end
  let l:user = input('user: ')
  let l:password = input('password: ')
  let l:encoded = s:Base64.encode(l:user . ':' . l:password)
  let l:header = 'Authorization: ' . l:method . ' ' . l:encoded
  call append(1 + len(l:request.headers), l:header)
endfunction

function! http#remove_header(header) abort
  call s:remove_header(a:header)
endfunction

function! http#set_header(header, value) abort
  call s:remove_header(a:header)
  let l:buffer = bufnr('')
  let l:lines = s:get_lines(l:buffer, -1, 0, 0)
  let l:request = s:parse_request(l:lines, s:uri_line_pattern, 0)
  call append(1 + len(l:request.headers), a:header.': '.a:value)
endfunction

" Teardown:{{{1
let &cpo = s:save_cpo
