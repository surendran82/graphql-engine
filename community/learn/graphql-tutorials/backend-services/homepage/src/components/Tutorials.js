import React from 'react';
import '../styles/styles.scss';
import CommonTutorialsList from './CommonTutorialsList';
import {frontendTutorial, backendTutorial, mobileTutorial} from './AllState.js'
class Tutorials extends React.Component {
  render() {
    return (
      <div className={'lightGrayBgColor commonSectionWrapper'}>
        <div className={'container noPadd'}>
          <div className={'tutorialWrapper'}>
            <div className={'col-md-12'}>
              <div className={'sectionHeader'}>
                Check out the tutorials
              </div>
            </div>
            <CommonTutorialsList
            title="Frontend Tutorials"
            subText="(2-hour Series)"
            frontendTutorial={frontendTutorial}
            />
            <CommonTutorialsList
            title="Backend Tutorials"
            subText=""
            frontendTutorial={backendTutorial}
            />
            <CommonTutorialsList
            title="Mobile Tutorials"
            subText="(2-hour Series)"
            frontendTutorial={mobileTutorial}
            />
          </div>
        </div>
      </div>
    );
  }
}

export default Tutorials;
