curl -v 'http://localhost:3000/threads?select=posts(*,attachments(*)),boards()&board_thread_id=eq.466060&posts.order=board_post_id.asc&boards.pathpart=eq.leftypol' \
    -H "Content-Type: application/json"
