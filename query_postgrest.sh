# curl -v 'http://localhost:3000/blocked_post_attachments?select=*,blocked_post(known_spam_attachments(hit_count,post_id))&blocked_post.known_spam_attachments.post_id=eq.1' \
curl -v 'http://localhost:3000/posts?select=*,attachments(board_filename,file_extension,thumb_extension)&thread_id=eq.13106&order=board_post_id.asc' \
     -X GET \
     -H "Content-Type: application/json"

