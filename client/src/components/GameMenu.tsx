import * as React from 'react'
import { Button } from './Button'
import { deleteStateFromStore } from '../state/localStorage';
import { GameIdContext } from '../App';
import { useStore } from 'react-redux';

import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faDownload, faUpload, faEraser } from '@fortawesome/free-solid-svg-icons'

function GameMenu() {

  const gameId = React.useContext(GameIdContext)
  const store = useStore()

  const [deleteMenu, setDeleteMenu] = React.useState(false);
  const openDeleteMenu = () => setDeleteMenu(true);
  const closeDeleteMenu = () => setDeleteMenu(false);

  const callDelete = () => {
    deleteStateFromStore()

    // TODO: Instead, delete the state `progress`
    window.location.reload()

    closeDeleteMenu()
  };
  const downloadProgress = () => {};
  // const uploadProgress = () => {};

  return <nav className="game-menu">
    <Button disabled={true} onClick={downloadProgress} title="Download game progress" to=""><FontAwesomeIcon icon={faDownload} /></Button>
    <Button disabled={true} title="Load game progress from JSON" to=""><FontAwesomeIcon icon={faUpload} /></Button>
    <Button title="Clear game progress" to="" onClick={openDeleteMenu}><FontAwesomeIcon icon={faEraser} /></Button>

    {deleteMenu?
      <div className="modal-wrapper">
      <div className="modal-backdrop" onClick={closeDeleteMenu} />
      <div className="modal">
        <div className="codicon codicon-close modal-close" onClick={closeDeleteMenu}></div>
        <h2>Delete Progress?</h2>

        <p>Do you want to delete your saved state irreversibly?</p>
        <p>(This only affects your saved proofs, no levels are ever locked.)</p>

        <Button onClick={callDelete} to="">Delete</Button>
        <Button disabled={true} onClick={downloadProgress && callDelete && closeDeleteMenu} to="">Download & Delete</Button>
        <Button onClick={closeDeleteMenu} to="">Cancel</Button>
      </div>
    </div> : null}
  </nav>
}

export default GameMenu
