; Props
(rule
 (targets gtkActionProps.ml ogtkActionProps.ml)
 (action (run propcc %{dep:gtkAction.props})))

(rule
 (targets gtkBaseProps.ml ogtkBaseProps.ml)
 (action (run propcc %{dep:gtkBase.props})))

(rule
 (targets gtkInvisibleProps.ml ogtkInvisibleProps.ml)
 (action (run propcc %{dep:gtkInvisible.props})))

(rule
 (targets gtkBuilderProps.ml ogtkBuilderProps.ml)
 (action (run propcc %{dep:gtkBuilder.props})))

; (rule
;  (targets gtkListProps.ml ogtkListProps.ml)
;  (action (run propcc %{dep:gtkList.props})))
