; Props
(rule
 (targets gtkBaseProps.ml ogtkBaseProps.ml)
 (action (run propcc %{dep:gtkBase.props})))

(rule
 (targets gtkInvisibleProps.ml ogtkInvisibleProps.ml)
 (action (run propcc %{dep:gtkInvisible.props})))
