const React = require('react');

const CompLibrary = require('../../core/CompLibrary.js');

const MarkdownBlock = CompLibrary.MarkdownBlock; /* Used to read markdown */
const Container = CompLibrary.Container;
const GridBlock = CompLibrary.GridBlock;

const siteConfig = require(`${process.cwd()}/siteConfig.js`);

function imgUrl(img) {
  return `${siteConfig.baseUrl}img/${img}`;
}

function docUrl(doc, language) {
  return `${siteConfig.baseUrl}docs/${language ? `${language}/` : ''}${doc}`;
}

function pageUrl(page, language) {
  return siteConfig.baseUrl + (language ? `${language}/` : '') + page;
}

const SplashContainer = props => (
  <div className="homeContainer shadow">
    <div className="homeSplashFade">
      <div className="wrapper homeWrapper">{props.children}</div>
    </div>
  </div>
);

const Logo = props => (
  <div className="projectLogo">
    <img src={props.img_src} alt="Project Logo" />
  </div>
);

const ProjectTitle = () => (
  <h2 className="projectTitle inverse">
    {siteConfig.title}
  </h2>
);

class HomeSplash extends React.Component {
  render() {
    const language = this.props.language || '';
    return (
      <SplashContainer>
        <div className="content">
          <div className="inner">
            <ProjectTitle />
        </div>
        <h2 className="inverseLight">Proptics is a Profunctor Optics and Lenses library for Scala</h2>
      </div>
      <div className="buttons">
        <a className="button" href="docs/overview">Getting Started</a>
        <a className="button" href="https://github.com/sagifogel/proptics">GITHUB</a>
      </div>

      </SplashContainer>
    );
  }
}

const Block = props => (
  <Container
    padding={['bottom', 'top']}
    id={props.id}
    background={props.background}>
    <GridBlock align="center" contents={props.children} layout={props.layout} />
  </Container>
);

const Features = () => (
  <Block layout="fourColumn">
    {[
      {
        title: 'Profunctional',
        content: 'Proptics uses a Profunctor encoding for the internal representation of its optics. You can learn what is profunctor in the [profunctor](/Proptics/docs/profunctors/profunctor) section.',
       },
      {
        title: 'Simple to use and easy to learn',
        content: 'Proptics API was designed to resemble the Scala standard library.',
       },
      {
        title: 'Cats Friendly',
        content: 'Proptics is built on top of [Cats](https://typelevel.org/cats/), [Cats-mtl](https://typelevel.org/cats-mtl/) and [Spire](https://typelevel.org/spire/)',
      },
    ]}
  </Block>
);

class Index extends React.Component {
  render() {
    const language = this.props.language || '';
    return (
      <div>
        <HomeSplash language={language} />
        <div className="mainContainer">
          <Features />
        </div>
      </div>
    );
  }
}

module.exports = Index;