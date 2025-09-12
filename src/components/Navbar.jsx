import React from 'react'
import { BrainCircuit, Sun, Moon } from 'lucide-react';

// This function returns a Navbar component
const Navbar = ({ onToggleTheme, theme }) => {
  // Return a JSX element
  return (
    <>
      <div
        className={`nav flex items-center justify-between h-[50px] ${theme === "dark" ? "bg-zinc-900" : "bg-gray-100"} rounded-2xl`}
        style={{ padding: "0px 150px", marginLeft: "15px", marginRight: "15px" , marginTop: "8px" }}
      >
        <div className="logo flex items-center gap-[10px]">
          <BrainCircuit size={30} color='#9333ea'/>
          <span className={`text-2xl font-bold ml-2 ${theme === "dark" ? "text-white" : "text-black"}`}>Codeify</span>
        </div>
        <div className="icons flex items-center gap-[20px]">
          <button
            className="flex items-center justify-center h-10 w-10 rounded-full cursor-pointer transition-all hover:text-[#9333ea]"
            onClick={onToggleTheme}
            title="Toggle theme"
          >
            {theme === "dark" ? <Sun /> : <Moon />}
          </button>
        </div>
      </div>
    </>
  )
}

export default Navbar