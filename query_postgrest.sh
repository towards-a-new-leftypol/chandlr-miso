curl -v 'http://localhost:3000/sites?select=*,boards(*,threads(*,posts(*,attachments(*))))&name=eq.leftychan&boards.pathpart=eq.ga&boards.threads.board_thread_id=eq.11787&boards.threads.posts.order=board_post_id.asc' \
    -H "Content-Type: application/json"
