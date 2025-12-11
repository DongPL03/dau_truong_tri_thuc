import {Injectable} from '@angular/core';
import {BehaviorSubject} from 'rxjs';

export type ToastType = 'info' | 'success' | 'error';

export interface ToastMessage {
  toast_id: number;
  message: string;
  type: ToastType;
  navigate_to?: any[]; // ['/ban-be'] hoặc ['/tran-dau', 7]
}

@Injectable({providedIn: 'root'})
export class ToastNotificationService {

  private readonly _toasts_subject =
    new BehaviorSubject<ToastMessage[]>([]);

  toasts$ = this._toasts_subject.asObservable();

  // ✅ thêm navigate_to
  show(
    message: string,
    type: ToastType = 'info',
    duration_ms = 4000,
    navigate_to?: any[]
  ): void {
    const toast: ToastMessage = {
      toast_id: Date.now(),
      message,
      type,
      navigate_to
    };

    const current = this._toasts_subject.value;
    this._toasts_subject.next([...current, toast]);

    if (duration_ms > 0) {
      setTimeout(() => this.dismiss(toast.toast_id), duration_ms);
    }
  }

  dismiss(toast_id: number): void {
    const current = this._toasts_subject.value;
    this._toasts_subject.next(
      current.filter(t => t.toast_id !== toast_id)
    );
  }

  clearAll(): void {
    this._toasts_subject.next([]);
  }
}
