defdtype ⟨char dd⟩
defdtype ⟨dd k b source⟩
defdtype ⟨dd* dd index⟩
defdtype ⟨yesno 'yes 'no⟩

atoms:
stringlit
numlit
dtype
symbol - quoted symbol
@symbol - evaluated symbol

--- or? --- (... I don't like this)
defdtype ⟨char @dd⟩
defdtype ⟨dd @k @b @source⟩
defdtype ⟨dd* @dd @index⟩
defdtype ⟨yesno yes no⟩


---

dtype ref
⟨char ⟨dd 1 param "c"⟩⟩
⟨yesno yes⟩
⟨yesno no⟩
⟨yesno @x⟩

