import { CommonModule } from '@angular/common';
import { Component, OnInit, inject, output, signal } from '@angular/core';
import Swal from 'sweetalert2';
import { DayReward, LoginStreakResponse } from '../../../models/login-streak.model';
import { LoginStreakService } from '../../../services/login-streak.service';

@Component({
  selector: 'app-login-streak-popup',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './login-streak-popup.html',
  styleUrl: './login-streak-popup.scss',
})
export class LoginStreakPopup implements OnInit {
  private loginStreakService = inject(LoginStreakService);

  // Output để đóng popup
  close = output<void>();

  loading = signal(true);
  claiming = signal(false);
  streakInfo = signal<LoginStreakResponse | null>(null);

  ngOnInit(): void {
    this.loadStreakInfo();
  }

  loadStreakInfo(): void {
    this.loading.set(true);
    this.loginStreakService.getStreakInfo().subscribe({
      next: (res) => {
        if (res.data) {
          this.streakInfo.set(res.data);
        }
        this.loading.set(false);
      },
      error: (err) => {
        console.error('Error loading streak info:', err);
        this.loading.set(false);
      },
    });
  }

  claimReward(): void {
    if (this.claiming() || this.streakInfo()?.da_diem_danh_hom_nay) return;

    this.claiming.set(true);
    this.loginStreakService.claimDailyReward().subscribe({
      next: (res) => {
        if (res.data) {
          this.streakInfo.set(res.data);

          Swal.fire({
            icon: 'success',
            title: '🎉 Điểm danh thành công!',
            html: this.formatRewardMessage(res.data),
            confirmButtonText: 'Tuyệt vời!',
            confirmButtonColor: '#6C5DD3',
          });
        }
        this.claiming.set(false);
      },
      error: (err) => {
        console.error('Error claiming reward:', err);
        this.claiming.set(false);
        Swal.fire('Lỗi', err.error?.message || 'Không thể điểm danh', 'error');
      },
    });
  }

  private formatRewardMessage(res: LoginStreakResponse): string {
    const reward = res.phan_thuong_hom_nay;
    if (!reward) {
      return res.thong_bao;
    }

    let html = `<div style="text-align: left; padding: 10px;">`;
    html += `<p>💰 <strong>+${reward.gold} Gold</strong></p>`;
    if (reward.xp > 0) {
      html += `<p>⭐ <strong>+${reward.xp} XP</strong></p>`;
    }
    if (reward.vat_pham_ten) {
      html += `<p>🎁 <strong>+${reward.so_luong_vat_pham} ${reward.vat_pham_ten}</strong></p>`;
    }
    html += `</div>`;
    return html;
  }

  getDayClass(day: DayReward): string {
    if (day.da_nhan) return 'day-claimed';
    if (day.la_hom_nay) return 'day-today';
    if (day.co_the_nhan) return 'day-available';
    return 'day-locked';
  }

  closePopup(): void {
    this.close.emit();
  }
}
