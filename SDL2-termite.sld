
(define-library (SDL2-termite)
  (export make-sdl2-node make-screen-node
          termitate-sdl2-node!)

  ; (import (scheme base))
  (import (gambit)) ;; define-macro

  (import (SDL2))
  (import (SDL2 ttf))

  (import (termite))
  (import (termite/utils))

  (begin
    (define *sdl-node* #f)

    (define (make-sdl2-node)
      (or *sdl-node*
          (begin
            (set! *sdl-node*
              (spawn
                (lambda ()
                  ;; uninitialized sdl2-node
                  (define (quit screens ttf)

                    (if ttf ;; ttf is enabled
                        (begin
                          (for-each
                            (lambda (name/font)
                              (TTF_Close (cdr name/font)))
                            ttf)
                          (TTF_Quit)))

                    (if screens ;; sdl is enabled
                        (begin
                          (for-each
                            (lambda (node)
                              (!? node 'quit))
                            screens)
                          (SDL_Quit))))

                  (let main-loop ((screens #f) (ttf #f))
                    (with-exception-handler
                      (lambda (exn)
                        (cond
                          ((termite-exception? exn)
                           (let* ((origin (termite-exception-origin exn))
                                  (new-screens (remove (lambda (obj)
                                                         (eq? origin obj))
                                                       screens)))
                             (if (null? new-screens)
                                 (quit new-screen ttf)
                                 (main-loop new-screens ttf))))
                          (else
                            ;;; Propagate that exception.
                            (raise exn))))
                      (lambda ()
                        (let wait-loop ()
                          (recv
                            (('init subsystem)
                             (SDL_Init subsystem)
                             (main-loop '() ttf))

                            (('init-ttf)
                             (TTF_Init)
                             (main-loop screens '()))

                            (('register screen-node)
                             (inbound-link screen-node))

                            (('open-font name size)
                             (main-loop screens
                                        (and ttf
                                             (cons
                                               (cons (cons name size)
                                                     (TTF_OpenFont name size))
                                               ttf))))

                            ((from tag ('get-font name size))
                             (! from (list tag (or ttf (assoc (cons name size) ttf))))
                             (loop screens ttf))

                            ('quit
                             (quit screens ttf))))))))))
            *sdl-node*)))

    (define (termitate-sdl2-node!)
      (if *sdl-node*
          (begin
            (! *sdl-node* 'quit)
            (set! *sdl-node* #f))))

    (define (make-screen-node title x y w h mode)
      (let* ((win (SDL_CreateWindow title x y w h mode))
             (renderer (SDL_CreateRenderer win -1 SDL_RENDERER_ACCELERATED)))
        (spawn
          (lambda ()
            (let loop ((timeout (/ 1 60.0))) ; 60 Hz
              (recv
                (('timeout val)
                 (loop val))

                ('toggle-cursor
                 (SDL_ToggleCursor renderer)
                 (loop timeout))

                ('clear
                 (SDL_RenderClear renderer)
                 (loop timeout))

                ((from tag 'quit)
                 (SDL_DestroyRenderer renderer)
                 (SDL_DestroyWindow win))

                (('set-draw-color color) (u8vector? color)
                 (SDL_SetRenderDrawColor renderer color)
                 (loop timeout))

                #;(('draw-text font text rect)
                 (TTF_RenderUTF8Solid font text))

                (('draw-rect rect) (where (s32vector? rect))
                 (SDL_RenderFillRect renderer rect)
                 (loop timeout))

                (msg
                  (pp msg)
                  (loop timeout))

                (after timeout
                  (SDL_RenderPresent renderer)
                  (loop timeout)))))
            name: (string->symbol title))))))

