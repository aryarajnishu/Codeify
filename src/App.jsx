import React, { useState, useEffect } from "react";
import "./App.css";
import Navbar from "./components/Navbar";
import Editor from "@monaco-editor/react";
import Select from "react-select";
import { GoogleGenAI } from "@google/genai";
import Markdown from "react-markdown";
import ReactMarkdown from "react-markdown";
import RingLoader from "react-spinners/RingLoader";

const boilerplates = {
  cpp: `// Online C++ compiler to run C++ program online
#include <bits/stdc++.h>
using namespace std;

int main() {
    // Write C++ code here
    cout << "AI Code Review Platform" << endl;

    return 0;
}
`,

  javascript: `
function main() {
  console.log("AI Code Review Platform");
}
main();
`,

  python: `
def main():
    print("AI Code Review Platform")

if __name__ == "__main__":
    main()
`,

  java: `
public class Main {
    public static void main(String[] args) {
        System.out.println("AI Code Review Platform");
    }
}
`,

  csharp: `
using System;
class Program {
    static void Main() {
        Console.WriteLine("AI Code Review Platform");
    }
}
`,

  php: `
<?php
echo "AI Code Review Platform";
?>
`,

  ruby: `
puts "AI Code Review Platform"
`,

  go: `
package main
import "fmt"
func main() {
    fmt.Println("AI Code Review Platform")
}
`,

  swift: `
import Foundation
print("AI Code Review Platform")
`,

  kotlin: `
fun main() {
    println("AI Code Review Platform")
}
`,

  typescript: `
function main(): void {
  console.log("AI Code Review Platform");
}
main();
`,

  rust: `
fn main() {
    println!("AI Code Review Platform");
}
`,

  dart: `
void main() {
  print('AI Code Review Platform');
}
`,

  scala: `
object Main extends App {
  println("AI Code Review Platform")
}
`,

  perl: `
print "AI Code Review Platform\\n";
`,

  haskell: `
main :: IO ()
main = putStrLn "AI Code Review Platform"
`,

  elixir: `
IO.puts "AI Code Review Platform"
`,

  r: `
cat("AI Code Review Platform\\n")
`,

  matlab: `
disp('AI Code Review Platform');
`,

  bash: `
echo "AI Code Review Platform"
`,
};

const App = () => {
  const options = [
    { value: "cpp", label: "C++" },
    { value: "python", label: "Python" },
    { value: "java", label: "Java" },
    { value: "csharp", label: "C#" },
    { value: "javascript", label: "JavaScript" },
    { value: "php", label: "PHP" },
    { value: "ruby", label: "Ruby" },
    { value: "go", label: "Go" },
    { value: "swift", label: "Swift" },
    { value: "kotlin", label: "Kotlin" },
    { value: "typescript", label: "TypeScript" },
    { value: "rust", label: "Rust" },
    { value: "dart", label: "Dart" },
    { value: "scala", label: "Scala" },
    { value: "perl", label: "Perl" },
    { value: "haskell", label: "Haskell" },
    { value: "elixir", label: "Elixir" },
    { value: "r", label: "R" },
    { value: "matlab", label: "MATLAB" },
    { value: "bash", label: "Bash" },
  ];

  const [selectedOption, setSelectedOption] = useState(options[0]);
  const [code, setCode] = useState("");
  const [loading, setLoading] = useState(false);
  const [response, setResponse] = useState("");
  const [theme, setTheme] = useState("dark"); // Add theme state

  const ai = new GoogleGenAI({
    apiKey: "AIzaSyD2FUQAZ693lwhvAcZmM7JrdUWZZVzfb4Y",
  });

  const customStyles = {
    control: (provided) => ({
      ...provided,
      backgroundColor: "#18181b",
      borderColor: "#3f3f46",
      color: "#fff",
      width: "100%",
      boxShadow: "0px 2px 8px rgba(0,0,0,0.5)",
      borderRadius: "12px",
    }),
    menu: (provided) => ({
      ...provided,
      backgroundColor: "#18181b",
      color: "#fff",
      width: "100%",
      borderRadius: "12px",
      boxShadow: "0px 4px 12px rgba(0,0,0,0.6)",
    }),
    singleValue: (provided) => ({
      ...provided,
      color: "#fff",
    }),
    option: (provided, state) => ({
      ...provided,
      backgroundColor: state.isFocused ? "#27272a" : "#18181b",
      color: "#fff",
      cursor: "pointer",
      padding: "10px 15px",
    }),
    input: (provided) => ({
      ...provided,
      color: "#fff",
    }),
    placeholder: (provided) => ({
      ...provided,
      color: "#a1a1aa",
    }),
  };

  async function fixCode() {
    setLoading(true);
    setResponse("");
    const response = await ai.models.generateContent({
      model: "gemini-2.0-flash",
      contents: `
      You are an expert-level software developer.
      Fix and improve the following ${selectedOption.value} code.

      Return your answer in this markdown format:

      ## Summary of Changes
      (Briefly explain what was changed and why.)

      ## Detailed Changes
      - For each change, write a clear sentence describing what was changed, where, and why. For example: "Changed variable name 'x' to 'count' on line 4 for clarity." Do not use a table.

      ## Fixed Code
      \`\`\`${selectedOption.value}
      (paste the corrected code here)
      \`\`\`

      Do not add any extra explanation outside this format.

      Code:
${code}
    `,
    });

    // Extract the fixed code from the markdown response
    const match = response.text.match(/```[\s\S]*?\n([\s\S]*?)```/);
    if (match && match[1]) {
      setCode(match[1].trim());
    }
    setResponse(response.text);
    setLoading(false);
  }

  async function reviewCode() {
    setResponse("");
    setLoading(true);
    const response = await ai.models.generateContent({
      model: "gemini-2.0-flash",
      contents: `You are an expert-level software developer reviewing ${selectedOption.value} code.
      Analyze it deeply, point out improvements, errors, bugs, and provide solutions.
      Code: ${code}`,
    });
    setResponse(response.text);
    setLoading(false);
  }

  const handleLanguageChange = (e) => {
    // If the code in the editor is the same as the previous language's boilerplate OR empty,
    // then replace it with the new language's boilerplate.
    const prevBoilerplate = boilerplates[selectedOption.value] || "";
    if (!code.trim() || code.trim() === prevBoilerplate.trim()) {
      setCode(boilerplates[e.value] || "");
    }
    setSelectedOption(e);
  };

  // Toggle theme handler
  const toggleTheme = () => {
    setTheme((prev) => (prev === "dark" ? "light" : "dark"));
    document.body.className = theme === "dark" ? "light" : "dark";
  };

  useEffect(() => {
    if (response) {
      console.log("Review Response:", response);
    }
  }, [response]);

  // Set default boilerplate for C++ on first load
  useEffect(() => {
    setCode(boilerplates["cpp"]);
  }, []);

  return (
    <>
      <Navbar onToggleTheme={toggleTheme} theme={theme} />
      <div
        className={`main flex gap-4 p-4 ${
          theme === "dark" ? "bg-zinc-950" : "bg-gray-100"
        }`}
        style={{ height: "calc(100vh - 90px)" }}
      >
        {/* LEFT SIDE - Code Editor */}
        <div
          className={`left flex flex-col w-1/2 h-full rounded-2xl shadow-2xl border-2
    ${
      theme === "dark"
        ? "bg-gradient-to-br from-zinc-900 via-zinc-800 to-zinc-950 border-zinc-800"
        : "bg-gradient-to-br from-white via-gray-100 to-gray-200 border-gray-200"
    }`}
          style={{ marginTop: "15px", marginLeft: "15px" }}
        >
          <div
            className={`tabs flex justify-center items-center gap-8 p-6 border-b-2 rounded-t-2xl shadow-md
      ${
        theme === "dark"
          ? "border-zinc-800 bg-gradient-to-r from-purple-900 via-zinc-950 to-indigo-900"
          : "border-gray-200 bg-gradient-to-r from-purple-100 via-gray-50 to-indigo-100"
      }`}
          >
            <Select
              value={selectedOption}
              onChange={handleLanguageChange}
              options={options}
              styles={customStyles}
              className="min-w-[200px] text-lg"
            />

            <button
              onClick={() =>
                code ? fixCode() : alert("Please enter code first")
              }
              className="h-9 min-w-[140px] text-lg font-semibold bg-gradient-to-r from-indigo-500 to-blue-600 text-white rounded-lg shadow transition-all hover:from-indigo-600 hover:to-blue-700"
            >
              Fix Code
            </button>

            <button
              onClick={() => {
                if (code) {
                  reviewCode();
                  setTimeout(() => {
                    if (response) {
                      console.log("Review Response:", response);
                    }
                  }, 1000); // Wait for response to update (adjust if needed)
                } else {
                  alert("Please enter code first");
                }
              }}
              className="h-10 min-w-[140px] text-base font-semibold bg-gradient-to-r from-green-500 to-emerald-600 text-white rounded-lg shadow transition-all hover:from-green-600 hover:to-emerald-700 flex items-center justify-center"
              style={{ height: "40px" }}
            >
              Review
            </button>
          </div>

          <div className="flex-1 overflow-hidden rounded-b-2xl">
            <Editor
              height="100%"
              theme={theme === "dark" ? "vs-dark" : "light"}
              language={selectedOption.value}
              value={code}
              onChange={(e) => setCode(e)}
              options={{
                fontSize: 16,
                fontFamily: "Fira Mono, Menlo, Monaco, Consolas, monospace",
                minimap: { enabled: false },
                scrollBeyondLastLine: false,
                smoothScrolling: true,
              }}
            />
          </div>
        </div>

        {/* RIGHT SIDE - Response */}
        <div
          className={`right w-1/2 h-full flex flex-col rounded-2xl shadow-2xl border-2  ${
            theme === "dark"
              ? "bg-gradient-to-br from-zinc-900 via-zinc-800 to-zinc-950 border-zinc-800"
              : "bg-gradient-to-br from-white via-gray-100 to-gray-200 border-gray-200"
          }`}
          style={{ marginTop: "15px", marginRight: "15px" }}
        >
          <div
            className={`topTab flex justify-center items-center px-6 h-[45px] rounded-t-2xl border-b-2  ${
              theme === "dark"
                ? "border-zinc-700 bg-zinc-950"
                : "border-gray-300 bg-gray-50"
            }`}
          >
            <p
              className={`font-extrabold text-xl tracking-wide ${
                theme === "dark" ? "text-white-900" : "text-white-900"
              }`}
            >
              Response
            </p>
          </div>

          <div
            className="flex-1 overflow-y-auto flex items-start justify-center transition-all duration-300 rounded-b-2xl"
            style={{
              minHeight: "200px",
              fontSize: "1.1rem",
              fontFamily: "Fira Mono, Menlo, Monaco, Consolas, monospace",
              boxShadow:
                theme === "dark"
                  ? "0 8px 32px 0 rgba(60, 60, 80, 0.37)"
                  : "0 8px 32px 0 rgba(180, 180, 200, 0.17)",
              borderRadius: "0 0 16px 16px",
              borderTop: "none",
              letterSpacing: "0.01em",
              wordBreak: "break-word",
              padding: "0.5rem 0.5rem 0.5rem 0.5rem",
            }}
          >
            {loading ? (
              <div className="flex justify-center items-center h-full w-full">
                <RingLoader color="#9333ea" />
              </div>
            ) : response && response.trim() !== "" ? (
              <div
                className={`prose ${
                  theme === "dark" ? "prose-invert" : ""
                } w-full text-left bg-white/10 rounded-xl shadow-lg`}
                style={{
                  padding: "2rem",
                  boxSizing: "border-box",
                  border:
                    theme === "dark"
                      ? "1.5px solid #444"
                      : "1.5px solid #d1d5db",
                  minHeight: "200px",
                  marginTop: "16px",
                }}
              >
                <ReactMarkdown>{response}</ReactMarkdown>
              </div>
            ) : null}
          </div>
        </div>
      </div>
    </>
  );
};

export default App;
