


(setq conn (json-rpc-connection "java -jar /home/yyoncho/.emacs.d/.cache/lsp/xmlls/org.eclipse.lemminx-0.18.0-uber.jar"))

(make-thread (lambda ()
               (json-rpc conn (lambda (result) (message "||| %s" result))
                         :object-type 'plist
                         :null-object nil
                         :false-object nil)))

(json-rpc-send conn "{}" )



(setq conn (json-rpc-connection "cat /home/yyoncho/Sources/nim/jsonrpc/test/fixtures/message.txt"))
