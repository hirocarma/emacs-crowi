<!---
/Spec/xxx
--->

# Request

# Pages API
client.page(page_id)
client.pages(path, user, offset)
client.create_page(path, body)
client.update_page(page_id, body)

# Comments API
client.comments(page_id)
client.add_comment(page_id, comment, revision_id)

# Attachments API
client.attachments(page_id)
client.add_attachment(page_id, filepath, content_type)

## POST (create,update)
 from buffer(get by crowi-get) file buffer file.

avoid secondary effect
for lsx list
from updatepost/bookmarks/likes to get
higher-order function

override defadvicce for Polymorphism

function StringValue(v:IstringValue){
    switch(v.class){ //オブジェクトが自分が何者かということを知っている。
    case Number: return StringValue-Number(number)
    case Date   : return StringValue-Date(date)
    }
}
###
  app.get('/_api/users.list'          , accessTokenParser , loginRequired(crowi, app) , user.api.list);
  app.get('/_api/pages.list'          , accessTokenParser , loginRequired(crowi, app) , page.api.list);
  app.post('/_api/pages.create'       , accessTokenParser , loginRequired(crowi, app) , csrf, page.api.create);
  app.post('/_api/pages.update'       , accessTokenParser , loginRequired(crowi, app) , csrf, page.api.update);
  app.get('/_api/pages.get'           , accessTokenParser , loginRequired(crowi, app) , page.api.get);
  app.get('/_api/pages.updatePost'    , accessTokenParser , loginRequired(crowi, app) , page.api.getUpdatePost);
  app.post('/_api/pages.seen'         , accessTokenParser , loginRequired(crowi, app) , page.api.seen);
  app.post('/_api/pages.rename'       , accessTokenParser , loginRequired(crowi, app) , csrf, page.api.rename);
  app.post('/_api/pages.remove'       , loginRequired(crowi, app) , csrf, page.api.remove); // (Avoid from API Token)
  app.post('/_api/pages.revertRemove' , loginRequired(crowi, app) , csrf, page.api.revertRemove); // (Avoid from API Token)
  app.post('/_api/pages.unlink'       , loginRequired(crowi, app) , csrf, page.api.unlink); // (Avoid from API Token)
  app.get('/_api/comments.get'        , accessTokenParser , loginRequired(crowi, app) , comment.api.get);
  app.post('/_api/comments.add'       , form.comment, accessTokenParser , loginRequired(crowi, app) , csrf, comment.api.add);
  app.get( '/_api/bookmarks.get'      , accessTokenParser , loginRequired(crowi, app) , bookmark.api.get);
  app.post('/_api/bookmarks.add'      , accessTokenParser , loginRequired(crowi, app) , csrf, bookmark.api.add);
  app.post('/_api/bookmarks.remove'   , accessTokenParser , loginRequired(crowi, app) , csrf, bookmark.api.remove);
  app.post('/_api/likes.add'          , accessTokenParser , loginRequired(crowi, app) , csrf, page.api.like);
  app.post('/_api/likes.remove'       , accessTokenParser , loginRequired(crowi, app) , csrf, page.api.unlike);
  app.get( '/_api/attachments.list'   , accessTokenParser , loginRequired(crowi, app) , attachment.api.list);
  app.post('/_api/attachments.add'    , uploads.single('file'), accessTokenParser, loginRequired(crowi, app) ,csrf, attachment.api.add);
  app.post('/_api/attachments.remove' , accessTokenParser , loginRequired(crowi, app) , csrf, attachment.api.remove);

### From buffer.Get by crowi-get

create dir local



(defclass some-userc nil
  ((id
	:initarg :id
	:initform (lambda () (some-user--make-uuid))
	:documentation "The id of the person")
   (name
	:type string
	:initarg :name
	:initform ""
	:documentation "Name of the person"))
  "A user record.")

(defun some-user--make-uuid () ; functions can be used by constructors
  "1213243")

(cl-defmethod some-user-greeting ((user some-userc) ; this is a type specifier
							   &optional daytime)
  "Methods are functions and have docstrings."
  (if daytime
	(message "good morning %s" (oref user name))))

(defun crowi-oop ()
  "Methods are functions and have docstrings."
  (interactive)
(let ((user (some-userc  :id "nic" :name "nic ferrier"))) ; make a user
  (some-user-greeting user t)) ; call the method
)


  
# Class



# Seq



# Reference


1.  [github][^1]
[^1]:https://github.com/hirocarma/drawio/blob/master/network.png?raw=true

# Note
