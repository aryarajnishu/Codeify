import React from 'react'
import { BrainCircuit, Sun, Moon } from 'lucide-react';

// This function returns a Navbar component
const Navbar = ({ onToggleTheme, theme }) => {
  // Return a JSX element
  return (
    <>
      <div className={`nav flex items-center justify-between h-[90px] ${theme === "dark" ? "bg-zinc-900" : "bg-gray-100"}`} style={{padding:"0px 150px"}}>
        <div className="logo flex items-center gap-[10px]">
          <BrainCircuit size={30} color='#9333ea'/>
          <span className={`text-2xl font-bold ml-2 ${theme === "dark" ? "text-white" : "text-black"}`}>Codeify</span>
        </div>
        <div className="icons flex items-center gap-[20px]">
          <i
            className='cursor-pointer transition-all hover:text-[#9333ea]'
            onClick={onToggleTheme}
            title="Toggle theme"
          >
            {theme === "dark" ? <Sun /> : <Moon />}
          </i>
        </div>
      </div>
    </>
  )
}

export default Navbar