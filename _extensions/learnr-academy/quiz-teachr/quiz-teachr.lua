-- Copied from moodlequiz/inst/webr.lua, sorry Mitch :3
function readTemplateFile(template)
  -- Establish a hardcoded path to where the .html partial resides
  -- Note, this should be at the same level as the lua filter.
  -- This is crazy fragile since Lua lacks a directory representation (!?!?)
  -- https://quarto.org/docs/extensions/lua-api.html#includes
  local path = quarto.utils.resolve_path(template)

  -- Let's hopefully read the template file...

  -- Open the template file
  local file = io.open(path, "r")

  -- Check if null pointer before grabbing content
  if not file then
    error("\nWe were unable to read the template file `" .. template .. "` from the extension directory.\n\n" ..
          "Double check that the extension is fully available by comparing the \n" ..
          "`_extensions/coatless/webr` directory with the main repository:\n" ..
          "https://github.com/coatless/quarto-webr/tree/main/_extensions/webr\n\n" ..
          "You may need to modify `.gitignore` to allow the extension files using:\n" ..
          "!_extensions/*/*/*\n")
    return nil
  end

  -- *a or *all reads the whole file
  local content = file:read "*a"

  -- Close the file
  file:close()

  -- Return contents
  return content
end

function substitute_in_file(contents, substitutions)

  -- Substitute values in the contents of the file
  contents = contents:gsub("{{%s*(.-)%s*}}", substitutions)

  -- Return the contents of the file with substitutions
  return contents
end

function quizTemplateFile()
  return readTemplateFile("quiz-template.html")
end

-- Define a function that escape control sequence
function escapeControlSequences(str)
  -- Perform a global replacement on the control sequence character
  return str:gsub("\\(%c)", "\\\\%1")
end

function prepSubstitutions(subs)
  return escapeControlSequences(table.concat(subs, "!@!"))
end

question_counter = 0

function Div(div)

  -- Find the divs that have the class quiz
  local has_schoice = div.classes:includes("quiz-singlechoice")
  local has_mchoice = div.classes:includes("quiz-multichoice")
  if has_schoice or has_mchoice then
    -- Log the output for debug

    local quiz_container = div.content[1]
    if quiz_container ~= nil then
      question_counter = question_counter + 1

      -- Might have to make these variables local later
      questions = {};
      solutions = {};
      local solution_index = 0;
      hints = {};

      for i, item in ipairs(quiz_container.content) do
        solution_index = solution_index + 1

        -- ah ha got a question

        item:walk {
          Span = function(span_block)
            table.insert(questions, pandoc.utils.stringify(span_block.content))
            table.insert(hints, span_block.attributes.hint)
          end
        }

        if(item[1].content[1].text == "☒") then
          table.insert(solutions, solution_index)
        end

      end

      local substitutions = {
        ["QUIZQUESTIONS"] = prepSubstitutions(questions),
        ["QUIZSOLUTIONS"] = prepSubstitutions(solutions),
        ["QUIZHINTS"] = prepSubstitutions(hints),
        ["QUIZCOUNTER"] = question_counter,
        ["QUIZTYPE"] = (has_mchoice and {"checkbox"} or {"radio"})[1]
      }

      code_template = quizTemplateFile()
      local quiz_cell = substitute_in_file(code_template, substitutions)

      return pandoc.RawInline('html', quiz_cell)
    end
  end
end
