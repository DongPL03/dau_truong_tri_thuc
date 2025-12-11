import {Component} from '@angular/core';
import {RouterModule, RouterOutlet} from '@angular/router';
import {ToastContainer} from '../components/toast-container/toast-container';

@Component({
  selector: 'app-root',
  imports: [
    RouterModule,
    RouterOutlet,
    ToastContainer
  ],
  templateUrl: './app.html',
  styleUrl: './app.scss',
  standalone: true
})
export class App {

}
