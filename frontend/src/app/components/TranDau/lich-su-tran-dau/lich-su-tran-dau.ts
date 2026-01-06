import {Component, OnInit} from '@angular/core';
import {CommonModule} from '@angular/common';
import {Base} from '../../base/base';
import {ResponseObject} from '../../../responses/response-object';
import {PageResponse} from '../../../responses/page-response';
import {LichSuTranDauResponse} from '../../../responses/trandau/lichsutrandau';
import {FormsModule} from '@angular/forms';

@Component({
  selector: 'app-lich-su-tran-dau',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './lich-su-tran-dau.html',
  styleUrl: './lich-su-tran-dau.scss',
})
export class LichSuTranDau extends Base implements OnInit {
  loading = true;
  page = 0;
  limit = 10;

  items: LichSuTranDauResponse[] = [];
  total_items = 0;
  total_pages = 0;
  filterRank: any;
  searchQuery: any;
  filterTime: any;

  ngOnInit(): void {
    this.loadData();
  }

  loadData(page: number = 0) {
    this.loading = true;
    this.tranDauService.getMyHistory(page, this.limit).subscribe({
      next: (res: ResponseObject<PageResponse<LichSuTranDauResponse>>) => {
        const p = res.data!;
        this.items = p.items;
        this.total_items = p.totalItems;
        this.total_pages = p.totalPages;
        this.loading = false;
      },
      error: () => {
        this.loading = false;
      }
    });
  }

  changePage(p: number) {
    if (p < 0 || p >= this.total_pages) return;
    this.page = p;
    this.loadData();
  }

  goToBattleDetail(tran_dau_id: number) {
    this.router.navigate(['/tran-dau/lich-su-tran-dau', tran_dau_id]).then();
  }

  getVisiblePages(): number[] {
    const visible: number[] = [];
    const maxVisible = 7; // số nút trang hiển thị tối đa
    const total = this.total_pages;

    if (total <= maxVisible) {
      return Array.from({length: total}, (_, i) => i);
    }

    const start = Math.max(0, this.page - 3);
    const end = Math.min(total - 1, this.page + 3);

    // luôn hiển thị trang đầu
    if (start > 0) visible.push(0);

    // nếu cách xa đầu -> thêm dấu ...
    if (start > 1) visible.push(-1);

    for (let i = start; i <= end; i++) visible.push(i);

    // nếu cách xa cuối -> thêm dấu ...
    if (end < total - 2) visible.push(-2);

    // luôn hiển thị trang cuối
    if (end < total - 1) visible.push(total - 1);

    return visible;
  }

  onSearch($event: any) {

  }

  onFilterChange() {

  }
}
