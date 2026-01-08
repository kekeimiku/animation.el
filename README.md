# animation.el

Just for fun

# Bracket content flash

`bracket-content-flash-mode` flashes (brief highlight then fade out) the content inside a bracket pair when point is immediately before the opening delimiter.

- Enable: `(bracket-content-flash-mode 1)`
- Customize: `bracket-content-flash-duration`, `bracket-content-flash-face`, `bracket-content-flash-blinks`, `bracket-content-flash-fade-steps`
- Note: delimiter detection uses the syntax table, plus optional `<>` support via `bracket-content-flash-include-angle-brackets`.
- Blink vs fade:
  - Default is fade out (`bracket-content-flash-blinks` = 0).
  - To enable blinking, set `bracket-content-flash-blinks` to a positive integer (e.g. 2).
- Troubleshooting:
  - It ignores strings/comments (by design).
  - It triggers when point is right before an opening delimiter (e.g. `(`/`[`/`{`/`<` when enabled).
  - It triggers on point movement/typing that leaves point right before an opening delimiter.
  - If you "see nothing", try increasing `bracket-content-flash-duration` and making `bracket-content-flash-face` more visible.
  - If it feels laggy while moving the cursor, set `bracket-content-flash-idle-delay` to a small value (e.g. 0.05–0.1) so it only flashes when Emacs is idle; default is 0 for immediate flashing.

# Example

<https://github.com/user-attachments/assets/7a3b9f8d-2605-4894-93e2-5232e955917c>

<https://github.com/user-attachments/assets/a8314755-9016-4ea2-96dc-2ac9caedaaa7>

<https://github.com/user-attachments/assets/b669385a-cbfb-4f8d-9a5c-aaf667119701>

<https://github.com/user-attachments/assets/a120c1da-defb-4b73-ba27-80835d6b25f3>

<https://github.com/user-attachments/assets/f4316542-60f1-4910-bb78-97a297d15703>

<https://github.com/user-attachments/assets/9d97888c-db4b-4ced-b88d-84a25fe50069>

<https://github.com/user-attachments/assets/718e2d23-32c4-440a-a953-dc1cbdaffd2a>

<https://github.com/user-attachments/assets/d6239a5c-f5f8-40a0-bd80-ff43e8ca331d>

<https://github.com/user-attachments/assets/88b0fa59-95bc-4216-9e3a-a94889e2e36b>
