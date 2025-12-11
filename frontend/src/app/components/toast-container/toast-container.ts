import {Component} from '@angular/core';
import {CommonModule} from '@angular/common';
import {ToastMessage} from '../../services/toast-notification.service';
import {Base} from '../base/base';

@Component({
  selector: 'app-toast-container',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './toast-container.html',
  styleUrl: './toast-container.scss'
})
export class ToastContainer extends Base {

  toasts$ = this.toastService.toasts$;


  close(toast: ToastMessage): void {
    this.toastService.dismiss(toast.toast_id);
  }

  // 👉 Click lên cả khung toast
  on_click_toast(toast: ToastMessage): void {
    if (toast.navigate_to) {
      this.router.navigate(toast.navigate_to).then();
    }
    this.close(toast);
  }

  // 👉 Click nút X chỉ đóng, không navigate
  on_click_close(event: MouseEvent, toast: ToastMessage): void {
    event.stopPropagation(); // không trigger click toast
    this.close(toast);
  }
}
