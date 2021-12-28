import "./style.css";
import main from "../output/Main";
import "../output/Components.Cycle";
main.main();

if (module.hot) {
	module.hot.accept("../output/Main", function () {
		document.body.innerHTML = "";
		main.main();
	});
	module.hot.accept("../output/Components.Cycle", function () {
		document.body.innerHTML = "";
		main.main();
	});
}
