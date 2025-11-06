import {Component} from '@angular/core';
import {RouterModule, RouterOutlet} from '@angular/router';

@Component({
  selector: 'app-root',
  imports: [
    RouterModule,
    RouterOutlet
  ],
  templateUrl: './app.html',
  styleUrl: './app.scss',
  standalone: true
})
export class App {

}
