--[[
  Verify reaper-input's mouse-modifier BEHAVIOR tables against the
  running REAPER, and find the entries we're missing.

  Why this exists: the tables in src/input/mouse_modifiers/behaviors/**
  (mirrored in action_names.rs) were hand-decoded from REAPER's docs and
  have drifted. Two rows are even marked "Duplicate in docs?", and the
  MIDI note CLICK table stops at 17 while REAPER 7.75 accepts ids up to
  19 -- so at least two behaviors exist that we have no name for.

  HOW IT WORKS
  GetMouseModifier() returns the ID ("5 m"), not a name, so it cannot
  enumerate anything. But SetMouseModifier() accepts a behavior NAME as
  well as an id. So we go the other way: assign each name we think we
  know, then read back which id REAPER resolved it to.

    - name sticks     -> confirms the name AND its true id
    - name bounces    -> our name is wrong (or not in this build)
    - id never claimed -> a behavior we have no name for; the script
                          lists these so you can read them off
                          Preferences > Mouse Modifiers

  HOW TO RUN
    REAPER > Actions > Show action list > ReaScript: Load...
    pick this file, then run it. It writes:
      <REAPER resource path>/mouse_behaviors_dump.txt

  SAFETY: the probe slot is saved and restored, and the whole run sits
  in an undo block. Your mouse map is left exactly as it was.
]]

-- The probe borrows modifier slot 0, then puts it back.
local PROBE_SLOT = 0

-- Highest id to scan when looking for unclaimed behaviors.
local MAX_ID = 40

-- Names we currently believe, straight out of action_names.rs, plus
-- CANDIDATES: guesses for the entries we know are missing. A candidate
-- that sticks is a real behavior; one that bounces just isn't spelled
-- that way.
local TABLES = {
  { ctx = 'MM_CTX_MIDI_NOTE_CLK', label = 'MIDI note left click', names = {
      'No action',
      'Select note',
      'Select note and move edit cursor',
      'Select note and move edit cursor ignoring snap',
      'Toggle note selection',
      'Add note to selection',
      'Erase note',
      'Toggle note mute',
      'Set note channel higher',
      'Set note channel lower',
      'Double note length',
      'Halve note length',
      'Select note and all later notes',
      'Add note and all later notes to selection',
      'Select note and all later notes of same pitch',
      'Add note and all later notes of same pitch to selection',
      'Select all notes in measure',
      'Add all notes in measure to selection',
      -- CANDIDATES for the unclaimed ids 18/19:
      'Add a range of notes into selection',
      'Add range of notes to selection',
      'Select a range of notes',
      'Add a range of notes to selection',
      'Select range of notes',
  }},
  { ctx = 'MM_CTX_MIDI_NOTE', label = 'MIDI note left drag', names = {
      'No action',
      'Move note',
      'Move note ignoring snap',
      'Erase notes',
      'Select time',
      'Move note on one axis only',
      'Move note on one axis only ignoring snap',
      'Copy note',
      'Copy note ignoring snap',
      'Edit note velocity',
      'Edit note velocity (fine)',
      'Move note horizontally',
      'Move note horizontally ignoring snap',
      'Move note vertically',
      'Select time ignoring snap',
      'Marquee select notes',
      'Marquee toggle note selection',
      'Marquee add to note selection',
      'Marquee select notes and time',
      'Marquee select notes and time ignoring snap',
      'Stretch note positions ignoring snap (arpeggiate)',
      'Stretch note selection vertically (arpeggiate)',
      'Stretch note lengths ignoring snap (arpeggiator legato)',
      'Stretch note lengths (arpeggiate legato)',
      'Copy note horizontally',
      'Copy note horizontally ignoring snap',
      'Copy note vertically',
      'Select notes touched while dragging',
      'Toggle selection for notes touched while dragging',
      'Move note ignoring selection',
      'Move note ignoring snap and selection',
      'Move note vertically ignoring scale/key',
      -- CANDIDATES for the snapping twin we're missing:
      'Stretch note positions (arpeggiate)',
      'Stretch note positions',
      'Stretch note lengths (arpeggiate)',
      'Stretch note selection vertically (arpeggiate) (duplicate)',
  }},
  { ctx = 'MM_CTX_MIDI_PIANOROLL', label = 'MIDI piano roll left drag', names = {
      'No action',
      'Insert note, drag to extend or change pitch',
      'Insert note ignoring snap, drag to extend or change pitch',
      'Erase notes',
      'Select time',
      'Paint notes and chords',
      'Select time ignoring snap',
      'Marquee select notes',
      'Marquee toggle note selection',
      'Marquee add to note selection',
      'Marquee select notes and time',
      'Marquee select notes and time ignoring snap',
      'Insert note, drag to move',
      'Paint a row of notes of the same pitch',
      'Insert note, drag to extend',
      'Insert note ignoring snap, drag to extend',
      'Scrub preview MIDI',
      'Insert note ignoring snap, drag to move',
      'Insert note ignoring snap, drag to edit velocity',
      'Insert note, drag to edit velocity',
      'Paint a stack of notes of the same time position',
      'Paint notes ignoring snap',
      'Paint notes',
      'Paint a straight line of notes',
      'Paint a straight line of notes ignoring snap',
      'Select notes touched while dragging',
      'Toggle selection for notes touched while dragging',
      'Copy selected notes',
      'Copy selected notes ignoring snap',
      'Move selected notes',
      'Move selected notes ignoring snap',
      'Insert note',
      'Insert note ignoring snap',
      'Insert note ignoring scale/key, drag to move',
      'Insert note ignoring snap and scale/key, drag to move',
      'Insert note ignoring scale/key, drag to extend or change pitch',
      'Insert note ignoring snap and scale/key, drag to extend or change pitch',
  }},
}

local out = {}
local function emit(s) out[#out + 1] = s end

-- GetMouseModifier hands back the ID string ("5 m"), never a name.
local function id_of(ctx)
  local v = reaper.GetMouseModifier(ctx, PROBE_SLOT, '')
  if type(v) ~= 'string' then v = tostring(v) end
  return (v:gsub('%s*m%s*$', ''):gsub('^%s+', ''):gsub('%s+$', ''))
end

emit('REAPER mouse-modifier behavior tables (name -> id round trip)')
emit('generated by reaper-input/scripts/dump_mouse_behaviors.lua')
emit('REAPER version: ' .. tostring(reaper.GetAppVersion()))
emit('')

reaper.Undo_BeginBlock()

for _, t in ipairs(TABLES) do
  local saved = reaper.GetMouseModifier(t.ctx, PROBE_SLOT, '')

  emit('[' .. t.ctx .. ']  -- ' .. t.label)

  -- 1. Which ids does REAPER accept at all? Setting an out-of-range id
  --    leaves the previous value in place, so a readback that doesn't
  --    echo what we set means the id is invalid.
  local valid = {}
  local top = -1
  for id = 0, MAX_ID do
    reaper.SetMouseModifier(t.ctx, PROBE_SLOT, tostring(id) .. ' m')
    if id_of(t.ctx) == tostring(id) then
      valid[id] = true
      top = id
    end
  end
  emit(string.format('  valid ids: 0..%d', top))
  emit('')

  -- 2. Round-trip each name we think we know.
  local claimed = {}
  emit('  name -> id')
  for _, name in ipairs(t.names) do
    -- Park on a known-different id so a no-op set is visible as a bounce.
    reaper.SetMouseModifier(t.ctx, PROBE_SLOT, '0 m')
    reaper.SetMouseModifier(t.ctx, PROBE_SLOT, name)
    local got = id_of(t.ctx)
    if got == '0' and name ~= 'No action' then
      emit(string.format('  %-64s  BOUNCED (name not recognised)', name))
    else
      claimed[tonumber(got) or -1] = name
      emit(string.format('  %-64s  %s', name, got))
    end
  end
  emit('')

  -- 3. Anything valid that no name claimed is a behavior we're missing.
  local missing = {}
  for id = 0, top do
    if valid[id] and not claimed[id] then missing[#missing + 1] = id end
  end
  if #missing > 0 then
    emit('  *** UNCLAIMED ids (we have no name for these) ***')
    emit('  ' .. table.concat(missing, ', '))
    emit('  Read them off Preferences > Mouse Modifiers > ' .. t.label)
  else
    emit('  (every valid id was claimed by a known name)')
  end
  emit('')

  reaper.SetMouseModifier(t.ctx, PROBE_SLOT, saved)
end

reaper.Undo_EndBlock('Probe mouse modifier behaviors (no-op)', -1)

local path = reaper.GetResourcePath() .. '/mouse_behaviors_dump.txt'
local f = io.open(path, 'w')
if f then
  f:write(table.concat(out, '\n'))
  f:close()
  reaper.ShowConsoleMsg('Wrote ' .. path .. '\n')
else
  reaper.ShowConsoleMsg(table.concat(out, '\n') .. '\n')
end
