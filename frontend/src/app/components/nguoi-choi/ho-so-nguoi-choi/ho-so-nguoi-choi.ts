import {Component, OnInit} from '@angular/core';
import {CommonModule} from '@angular/common';
import {ActivatedRoute} from '@angular/router';
import {Base} from '../../base/base';
import {UserSummaryResponse} from '../../../responses/nguoidung/user-summary-response';
import {LichSuTranDauResponse} from '../../../responses/trandau/lichsutrandau';
import {ResponseObject} from '../../../responses/response-object';
import {PageResponse} from '../../../responses/page-response';


@Component({
  selector: 'app-ho-so-nguoi-choi',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './ho-so-nguoi-choi.html',
  styleUrl: './ho-so-nguoi-choi.scss'
})
export class HoSoNguoiChoi extends Base implements OnInit {

  user_id!: number;

  loading_summary = false;
  loading_history = false;

  summary?: UserSummaryResponse | null;
  history_items: LichSuTranDauResponse[] = [];
  history_page = 0;
  history_limit = 10;
  history_total_pages = 0;


  ngOnInit() {
    this.route.paramMap.subscribe(params => {
      const idParam = params.get('id');
      if (idParam) {
        this.user_id = Number(idParam);
        this.loadSummary();
        this.loadHistory(0);
      }
    });
  }

  loadSummary() {
    this.loading_summary = true;
    this.userService.getUserSummary(this.user_id).subscribe({
      next: (res: ResponseObject<UserSummaryResponse>) => {
        this.loading_summary = false;
        this.summary = res.data ?? null;
      },
      error: err => {
        this.loading_summary = false;
        this.handleApiError(err);
      }
    });
  }

  loadHistory(page: number) {
    this.loading_history = true;
    this.tranDauService.getUserHistory(this.user_id, page, this.history_limit)
      .subscribe({
        next: (res: ResponseObject<PageResponse<LichSuTranDauResponse>>) => {
          this.loading_history = false;
          const data = res.data!;
          this.history_items = data.items ?? [];
          this.history_page = data.currentPage ?? 0;
          this.history_total_pages = data.totalPages ?? 0;
        },
        error: err => {
          this.loading_history = false;
          this.handleApiError(err);
        }
      });
  }

  changeHistoryPage(p: number) {
    if (p < 0 || p >= this.history_total_pages) {
      return;
    }
    this.loadHistory(p);
  }

  isMe(): boolean {
    const me = this.userService.getUserResponseFromLocalStorage();
    return !!me && me.id === this.user_id;
  }

  private handleApiError(err: any) {
    console.error('API Error:', err);
  }
}
