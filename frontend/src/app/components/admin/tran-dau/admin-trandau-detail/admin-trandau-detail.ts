import {Component, OnInit} from '@angular/core';
import {CommonModule} from '@angular/common';
import Swal from 'sweetalert2';
import {Base} from '../../../base/base';
import {ResponseObject} from '../../../../responses/response-object';


@Component({
  selector: 'app-admin-trandau-detail',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './admin-trandau-detail.html',
  styleUrl: './admin-trandau-detail.scss'
})

export class AdminTrandauDetail extends Base implements OnInit {
  loading = false;
  lichSuId!: number;

  detail: any = null;
  leaderboard: any[] = [];
  questions: any[] = [];

  // --- Modal 1: theo người chơi ---
  show_player_modal = false;
  player_modal_loading = false;
  player_modal_title = '';
  player_answers: any[] = [];

  // --- Modal 2: theo câu hỏi ---
  show_question_modal = false;
  question_modal_loading = false;
  question_modal: any = null;

  ngOnInit(): void {
    const idParam = this.route.snapshot.paramMap.get('lichSuId');
    this.lichSuId = idParam ? +idParam : 0;
    if (!this.lichSuId) {
      Swal.fire('Lỗi', 'Không xác định được bản ghi lịch sử', 'error')
        .then(() => this.router.navigate(['/admin/tran-dau']));
      return;
    }
    this.loadDetail();
  }

  loadDetail(): void {
    this.loading = true;
    this.tranDauService.getHistoryDetailAdmin(this.lichSuId).subscribe({
      next: (res: ResponseObject<any>) => {
        this.detail = res.data || null;
        this.leaderboard = this.detail?.leaderboard ?? [];
        this.questions = this.detail?.questions ?? [];
        this.loading = false;
      },
      error: () => {
        this.loading = false;
        Swal.fire('Lỗi', 'Không thể tải chi tiết lịch sử trận đấu', 'error')
          .then(() => this.router.navigate(['/admin/tran-dau']));
      }
    });
  }

  goBack(): void {
    this.router.navigate(['/admin/tran-dau']).then(r => {
    });
  }

  openPlayerModal(p: any): void {
    const userId = p.userId ?? p.user_id;
    if (!userId || !this.detail?.tran_dau_id) {
      return;
    }

    this.show_player_modal = true;
    this.player_modal_loading = true;
    this.player_modal_title =
      `${p.hoTen ?? p.ho_ten ?? 'Người chơi'} – trận #${this.detail.tran_dau_id}`;
    this.player_answers = [];

    this.tranDauService
      .getPlayerAnswersAdmin(this.detail.tran_dau_id, userId)
      .subscribe({
        next: (res: ResponseObject<any>) => {
          this.player_answers = res.data ?? [];
          this.player_modal_loading = false;
        },
        error: (err: any) => {
          console.error('getPlayerAnswersAdmin error', err);
          this.player_modal_loading = false;
        }
      });
  }

  closePlayerModal(): void {
    this.show_player_modal = false;
  }

  openQuestionModal(q: any): void {
    const cauHoiId = q.cau_hoi_id ?? q.cauHoiId;
    if (!cauHoiId || !this.detail?.tran_dau_id) {
      return;
    }

    this.show_question_modal = true;
    this.question_modal_loading = true;
    this.question_modal = null;

    this.tranDauService
      .getQuestionAnswersAdmin(this.detail.tran_dau_id, cauHoiId)
      .subscribe({
        next: (res: ResponseObject<any>) => {
          this.question_modal = res.data;
          console.log('question_modal', this.question_modal);
          this.question_modal_loading = false;
        },
        error: (err: any) => {
          console.error('getQuestionAnswersAdmin error', err);
          this.question_modal_loading = false;
        }
      });
  }

  closeQuestionModal(): void {
    this.show_question_modal = false;
  }
}
